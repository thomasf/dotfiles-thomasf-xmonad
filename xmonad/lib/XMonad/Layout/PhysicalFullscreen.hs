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

newtype PhysicalFullscreenState = PhysicalFullscreenState (Maybe PhysicalFullscreenInfo)
  deriving (Show, Read)

instance ExtensionClass PhysicalFullscreenState where
  initialValue = PhysicalFullscreenState Nothing

-- | Reset any active physical fullscreen window back to its original location (tiled in its pane).
resetPhysicalFullscreen :: X ()
resetPhysicalFullscreen = do
  PhysicalFullscreenState mInfo <- XS.get
  whenJust mInfo $ \info -> do
    XS.put (PhysicalFullscreenState Nothing)
    let w = pfiWindow info
    setFullscreen w False
    bw <- asks (borderWidth . config)
    withDisplay $ \d -> io $ setWindowBorderWidth d w (fromIntegral bw)
    isStillFlt <- gets (M.member w . W.floating . windowset)
    when isStillFlt $ windows (W.sink w)

-- | Reset physical fullscreen if the given window is not the currently fullscreen window.
resetPhysicalFullscreenIfOther :: Window -> X ()
resetPhysicalFullscreenIfOther w = do
  PhysicalFullscreenState mInfo <- XS.get
  when (Just w /= fmap pfiWindow mInfo) resetPhysicalFullscreen

-- | Reset physical fullscreen if active window on the workspace changes,
-- or the workspace is hidden, moved, or modified.
checkPhysicalFullscreenReset :: X ()
checkPhysicalFullscreenReset = do
  PhysicalFullscreenState mInfo <- XS.get
  whenJust mInfo $ \info -> do
    ws <- gets windowset
    let w           = pfiWindow info
        tag         = pfiWorkspace info
        mScreen     = find ((== tag) . W.tag . W.workspace) (W.screens ws)
        isStillFlt  = M.member w (W.floating ws)
        isStillWin  = w `W.member` ws

        shouldReset = case mScreen of
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

    when shouldReset resetPhysicalFullscreen

-- | Navigate screens, skipping any virtual panes that are covered by an active physical fullscreen window,
-- and ensuring that navigating onto a physical fullscreen monitor focuses the fullscreen window.
physicalScreenGo :: Nav2d.Direction2D -> Bool -> X ()
physicalScreenGo dir wrap = do
  PhysicalFullscreenState mInfo <- XS.get
  case mInfo of
    Nothing -> Nav2d.screenGo dir wrap
    Just info -> do
      ws <- gets windowset
      let curScreen = W.current ws
          curPane   = screenRect (W.screenDetail curScreen)
          Rectangle cx cy _ _ = curPane
          startedOnPhys = pointWithin (cx + 1) (cy + 1) (pfiPhysRect info)

      -- Navigate one screen
      Nav2d.screenGo dir wrap

      -- If we started on the physical fullscreen monitor, skip past any other panes on this same monitor
      when startedOnPhys $ do
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
          landedOnPhys = pointWithin (xF + 1) (yF + 1) (pfiPhysRect info)
      when landedOnPhys $ do
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
    isFull <- runQuery isFullscreen w
    if isFull
      then do
        withDisplay $ \dpy -> withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
          setEventType ev configureNotify
          setConfigureEvent ev w w (wa_x wa) (wa_y wa) (wa_width wa) (wa_height wa) 0 none (wa_override_redirect wa)
          sendEvent dpy w False 0 ev
        return (All False)
      else mempty

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
  let cx = fromIntegral vx + fromIntegral vw `div` 2
      cy = fromIntegral vy + fromIntegral vh `div` 2
      physRect@(Rectangle px py pw ph) = fromMaybe vRect $ find (pointWithin cx cy) physScreens

      rx = toRational (px - vx) / toRational vw
      ry = toRational (py - vy) / toRational vh
      rw = toRational pw / toRational vw
      rh = toRational ph / toRational vh
      targetRRect = W.RationalRect rx ry rw rh

      isFull = case M.lookup w (W.floating ws) of
        Just r  -> r == targetRRect
        Nothing -> False

  if isFull
    then resetPhysicalFullscreen
    else do
      XS.put (PhysicalFullscreenState $ Just PhysicalFullscreenInfo
        { pfiWindow     = w
        , pfiWorkspace  = curTag
        , pfiScreen     = curScId
        , pfiPaneRect   = vRect
        , pfiPhysRect   = physRect
        , pfiLayoutDesc = curLayout
        })
      setFullscreen w True
      withDisplay $ \d -> io $ do
        setWindowBorderWidth d w 0
        raiseWindow d w
      windows $ W.float w targetRRect
