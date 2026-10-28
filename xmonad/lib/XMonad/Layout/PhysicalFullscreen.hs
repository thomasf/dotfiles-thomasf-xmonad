{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PhysicalFullscreen
-- Description :  Fullscreen across entire physical monitor spanning split panes
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Toggle fullscreen over the entire physical screen, even if the screen
-- is split into panes with LayoutScreens, while respecting size hints,
-- GTK/CSD, EWMH standards, and screen navigation.
-----------------------------------------------------------------------------
module XMonad.Layout.PhysicalFullscreen
    ( PhysicalFullscreenInfo(..)
    , PhysicalFullscreenState(..)
    , toggleFullPhysicalScreen
    , resetPhysicalFullscreen
    , resetPhysicalFullscreenWindow
    , resetAllPhysicalFullscreen
    , resetPhysicalFullscreenIfOther
    , checkPhysicalFullscreenReset
    , fullscreenConfigureHook
    , physicalScreenGo
    , setFullscreen
    ) where

import           Control.Monad
import           Data.List (delete, find, nub)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (All(..))

import           XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.WindowProperties (getProp32)
import           XMonad.Hooks.ManageHelpers (isFullscreen)
import qualified XMonad.Actions.Navigation2D as Nav2d

-- | Information tracked about an active physical fullscreen window.
data PhysicalFullscreenInfo = PhysicalFullscreenInfo
  { pfiWindow     :: !Window
  , pfiWorkspace  :: !WorkspaceId
  , pfiScreen     :: !ScreenId
  , pfiPaneRect   :: !Rectangle
  , pfiPhysRect   :: !Rectangle
  , pfiLayoutDesc :: !String
  } deriving (Show, Read)

newtype PhysicalFullscreenState = PhysicalFullscreenState (M.Map Window PhysicalFullscreenInfo)
  deriving (Show, Read)

instance ExtensionClass PhysicalFullscreenState where
  initialValue  = PhysicalFullscreenState M.empty
  extensionType = PersistentExtension

-- | The center of a rectangle.
rectCenter :: Rectangle -> (Position, Position)
rectCenter (Rectangle x y w h) = (x + fromIntegral w `div` 2, y + fromIntegral h `div` 2)

-- | Reset the physical fullscreen windows on the physical screen holding the
-- center of the given pane. The tracked physical rectangles tell which
-- screen that is, without asking Xinerama.
resetPhysicalFullscreenAt :: Rectangle -> X ()
resetPhysicalFullscreenAt pane = do
  PhysicalFullscreenState m <- XS.get
  let (cx, cy) = rectCenter pane
  mapM_ resetPhysicalFullscreenWindow
    [w | (w, info) <- M.toList m, pointWithin cx cy (pfiPhysRect info)]

-- | Helper to restore window borders, remove fullscreen EWMH atom, and sink to tiled layer.
restoreWindow :: Window -> X ()
restoreWindow w = do
  setFullscreen w False
  bw <- asks (borderWidth . config)
  withDisplay $ \d -> io $ setWindowBorderWidth d w (fromIntegral bw)
  isStillFlt <- gets (M.member w . W.floating . windowset)
  when isStillFlt $ windows (W.sink w)

-- | Reset a specific physical fullscreen window.
resetPhysicalFullscreenWindow :: Window -> X ()
resetPhysicalFullscreenWindow w = do
  PhysicalFullscreenState m <- XS.get
  when (M.member w m) $ XS.put (PhysicalFullscreenState $ M.delete w m)
  restoreWindow w

-- | Reset all active physical fullscreen windows across all physical monitors.
resetAllPhysicalFullscreen :: X ()
resetAllPhysicalFullscreen = do
  PhysicalFullscreenState m <- XS.get
  XS.put (PhysicalFullscreenState M.empty)
  mapM_ restoreWindow (M.keys m)

-- | Reset physical fullscreen window on the current physical screen (if any).
resetPhysicalFullscreen :: X ()
resetPhysicalFullscreen =
  resetPhysicalFullscreenAt . screenRect . W.screenDetail . W.current =<< gets windowset

-- | Reset physical fullscreen if the given window is not the currently fullscreen window
-- on that window's physical screen.
resetPhysicalFullscreenIfOther :: Window -> X ()
resetPhysicalFullscreenIfOther w = do
  PhysicalFullscreenState m <- XS.get
  unless (M.null m || M.member w m) $ do
    ws <- gets windowset
    whenJust (find (\s -> w `elem` (W.integrate' . W.stack . W.workspace $ s)) (W.screens ws)) $
      resetPhysicalFullscreenAt . screenRect . W.screenDetail

-- | Reset physical fullscreen if active window on the workspace changes,
-- or the workspace is hidden, moved, or modified.
checkPhysicalFullscreenReset :: X ()
checkPhysicalFullscreenReset = do
  PhysicalFullscreenState m <- XS.get
  unless (M.null m) $ do
    ws <- gets windowset
    let shouldReset info =
          let w           = pfiWindow info
              tag         = pfiWorkspace info
              mScreen     = find ((== tag) . W.tag . W.workspace) (W.screens ws)
              isStillFlt  = M.member w (W.floating ws)
              isStillWin  = w `W.member` ws
          in case mScreen of
            Nothing -> True  -- workspace hidden
            Just sc ->
              let wsp         = W.workspace sc
                  activeWin   = fmap W.focus (W.stack wsp)
                  curPaneRect = screenRect (W.screenDetail sc)
                  curLayout   = description (W.layout wsp)
              in not isStillWin
                 || not isStillFlt
                 || activeWin /= Just w                   -- active window in same workspace changed
                 || W.screen sc /= pfiScreen info          -- workspace moved to another screen
                 || curPaneRect /= pfiPaneRect info        -- pane split / geometry changed
                 || curLayout /= pfiLayoutDesc info        -- layout changed

        toReset = [w | (w, info) <- M.toList m, shouldReset info]
    mapM_ resetPhysicalFullscreenWindow toReset

-- | Navigate screens, skipping any virtual panes that are covered by an active physical fullscreen window,
-- and ensuring that navigating onto a physical fullscreen monitor focuses the fullscreen window.
physicalScreenGo :: Nav2d.Direction2D -> Bool -> X ()
physicalScreenGo dir wrap = do
  PhysicalFullscreenState m <- XS.get
  let activeInfos = M.elems m
  if null activeInfos
    then Nav2d.screenGo dir wrap
    else do
      ws <- gets windowset
      let curScreen = W.current ws
          curPane   = screenRect (W.screenDetail curScreen)
          Rectangle cx cy _ _ = curPane
          mCurrentPhysInfo = find (\info -> pointWithin (cx + 1) (cy + 1) (pfiPhysRect info)) activeInfos

      -- Navigate one screen
      Nav2d.screenGo dir wrap

      -- If we started on the physical fullscreen monitor, skip past any other panes on this same monitor
      whenJust mCurrentPhysInfo $ \info -> do
        let loop n = when (n > (0 :: Int)) $ do
              ws' <- gets windowset
              let s' = W.current ws'
                  r' = screenRect (W.screenDetail s')
                  Rectangle x' y' _ _ = r'
                  stillOnPhys = pointWithin (x' + 1) (y' + 1) (pfiPhysRect info)
                  curTag = W.tag (W.workspace s')
              when (stillOnPhys && curTag /= pfiWorkspace info) $ do
                Nav2d.screenGo dir wrap
                loop (n - 1)
        loop (length (W.screens ws))

      -- Ensure that if focus ended up on this physical monitor, the fullscreen window is focused
      wsFinal <- gets windowset
      let sFinal = W.current wsFinal
          rFinal = screenRect (W.screenDetail sFinal)
          Rectangle xF yF _ _ = rFinal
          mLandedPhysInfo = find (\info -> pointWithin (xF + 1) (yF + 1) (pfiPhysRect info)) activeInfos
      whenJust mLandedPhysInfo $ \info -> do
        windows (W.view (pfiWorkspace info) . W.focusWindow (pfiWindow info))
        withDisplay $ \d -> io $ raiseWindow d (pfiWindow info)

-- | Set or remove _NET_WM_STATE_FULLSCREEN on a window.
setFullscreen :: Window -> Bool -> X ()
setFullscreen w full = withDisplay $ \dpy -> do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc  <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate  <- fromMaybe [] <$> getProp32 wmstate w
  let wstate' = if full
                then nub (fromIntegral fullsc : wstate)
                else delete (fromIntegral fullsc) wstate
  io $ changeProperty32 dpy w wmstate aTOM propModeReplace wstate'

-- | Prevent floating fullscreen windows (such as Emacs or Nautilus) from losing their
-- fullscreen geometry or regaining borders when they request character-cell or GTK sizing.
fullscreenConfigureHook :: Event -> X All
fullscreenConfigureHook ConfigureRequestEvent { ev_window = w } = do
    PhysicalFullscreenState m <- XS.get
    if M.member w m
      then notifyConfigure
      else do
        isFull <- runQuery isFullscreen w
        if isFull then notifyConfigure else mempty
  where
    notifyConfigure = do
      withDisplay $ \dpy -> withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
        setEventType ev configureNotify
        setConfigureEvent ev w w (wa_x wa) (wa_y wa) (wa_width wa) (wa_height wa) 0 none (wa_override_redirect wa)
        sendEvent dpy w False 0 ev
      return (All False)

fullscreenConfigureHook _ = mempty

-- | Toggle fullscreen over the entire physical screen, even if the screen is split into panes with LayoutScreens.
toggleFullPhysicalScreen :: X ()
toggleFullPhysicalScreen = withFocused $ \w -> do
  ws <- gets windowset
  let curScreen = W.current ws
      curWsp    = W.workspace curScreen
      curTag    = W.tag curWsp
      curScId   = W.screen curScreen
      vRect@(Rectangle vx vy vw vh) = screenRect . W.screenDetail $ curScreen
      curLayout = description (W.layout curWsp)

  physScreens <- withDisplay getCleanedScreenInfo
  let (cx, cy) = rectCenter vRect
      physRect@(Rectangle px py pw ph) = fromMaybe vRect $ find (pointWithin cx cy) physScreens

      rx = toRational (px - vx) / toRational vw
      ry = toRational (py - vy) / toRational vh
      rw = toRational pw / toRational vw
      rh = toRational ph / toRational vh
      targetRRect = W.RationalRect rx ry rw rh

  PhysicalFullscreenState m <- XS.get
  let isTracked = M.member w m
      isFull = isTracked || case M.lookup w (W.floating ws) of
        Just r  -> r == targetRRect
        Nothing -> False

  if isFull
    then do
      if isTracked
        then resetPhysicalFullscreenWindow w
        else restoreWindow w
    else do
      -- If there is already another window fullscreened on this same physical screen, reset that window first
      let onSamePhys = [pw | (pw, pinfo) <- M.toList m, pfiPhysRect pinfo == physRect]
      mapM_ resetPhysicalFullscreenWindow onSamePhys

      XS.modify $ \(PhysicalFullscreenState st) ->
        PhysicalFullscreenState $ M.insert w PhysicalFullscreenInfo
          { pfiWindow     = w
          , pfiWorkspace  = curTag
          , pfiScreen     = curScId
          , pfiPaneRect   = vRect
          , pfiPhysRect   = physRect
          , pfiLayoutDesc = curLayout
          } st
      setFullscreen w True
      withDisplay $ \d -> io $ do
        setWindowBorderWidth d w 0
        raiseWindow d w
      windows $ W.float w targetRRect
