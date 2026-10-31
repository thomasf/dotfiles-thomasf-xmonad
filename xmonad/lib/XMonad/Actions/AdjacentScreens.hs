-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.AdjacentScreens
-- Description :  Directional window and screen navigation without skipping empty screens
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides 2D navigation that traverses windows on the current screen first,
-- and then moves to the immediately adjacent screen (Up, Down, Left, Right)
-- without skipping empty screens. Also provides hierarchical upper/lower
-- row navigation, screen swaps, and window drags.
-----------------------------------------------------------------------------

module XMonad.Actions.AdjacentScreens
    ( ScreenNavConfig(..)
    , defaultScreenNavConfig
    , focusUp
    , focusDown
    , focusLeft
    , focusRight
    , windowSwapUp
    , windowSwapDown
    , windowSwapLeft
    , windowSwapRight
    , screenSwapUp
    , screenSwapDown
    , screenSwapLeft
    , screenSwapRight
    , windowToScreenUp
    , windowToScreenDown
    , windowToScreenLeft
    , windowToScreenRight
    ) where

import           Control.Monad (when)
import           Data.List (find, sortOn)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Ord (Down(..))

import           XMonad hiding (D)
import           XMonad.Actions.Navigation2D (Direction2D(..))
import qualified XMonad.Actions.Navigation2D as Nav2d
import qualified XMonad.StackSet as W

-- | Configuration for directional screen and window navigation.
data ScreenNavConfig = ScreenNavConfig
  { navBeforeScreenChange :: X ()                  -- ^ Action run before switching to another screen (e.g. showWorkspaceNameOld)
  , navAfterScreenChange  :: X ()                  -- ^ Action run after switching to another screen (e.g. showWorkspaceNameFast)
  , navFocusPointer       :: X ()                  -- ^ Action to focus/warp pointer to current window or screen center
  , navIsUpperScreen      :: Rectangle -> Bool     -- ^ Predicate identifying upper row screens
  , navSingleWindowDown   :: Maybe (Rectangle -> Bool)
    -- ^ Moving down from a workspace with at most one window goes to the leftmost
    -- screen below matching this, instead of the closest one
  }

-- | Default navigation configuration.
defaultScreenNavConfig :: ScreenNavConfig
defaultScreenNavConfig = ScreenNavConfig
  { navBeforeScreenChange = return ()
  , navAfterScreenChange  = return ()
  , navFocusPointer       = return ()
  , navIsUpperScreen      = (== 0) . rect_y
  , navSingleWindowDown   = Nothing
  }

type NavScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- | Move focus: within the workspace first, then to the adjacent screen.
focusUp, focusDown, focusLeft, focusRight :: ScreenNavConfig -> X ()
focusUp    = focusDir U
focusDown  = focusDir D
focusLeft  = focusDir L
focusRight = focusDir R

-- | Swap the focused window: within the workspace first, then with the
-- focused window of the adjacent screen, or move it there if it is empty.
windowSwapUp, windowSwapDown, windowSwapLeft, windowSwapRight :: ScreenNavConfig -> X ()
windowSwapUp    = windowSwapDir U
windowSwapDown  = windowSwapDir D
windowSwapLeft  = windowSwapDir L
windowSwapRight = windowSwapDir R

-- | Swap the workspace with the one on the adjacent screen.
screenSwapUp, screenSwapDown, screenSwapLeft, screenSwapRight :: ScreenNavConfig -> X ()
screenSwapUp    = screenSwapDir U
screenSwapDown  = screenSwapDir D
screenSwapLeft  = screenSwapDir L
screenSwapRight = screenSwapDir R

-- | Drag the focused window to the adjacent screen and follow it.
windowToScreenUp, windowToScreenDown, windowToScreenLeft, windowToScreenRight :: ScreenNavConfig -> X ()
windowToScreenUp    = toScreenDir U (\_ t -> W.view t . W.shift t)
windowToScreenDown  = toScreenDir D (\_ t -> W.view t . W.shift t)
windowToScreenLeft  = toScreenDir L (\_ t -> W.view t . W.shift t)
windowToScreenRight = toScreenDir R (\_ t -> W.view t . W.shift t)

-- | The screen adjacent to the current one in the direction. Up goes to
-- the upper row screen horizontally closest to the focused window. Down
-- only leaves the upper row, to the closest screen below, or the one
-- navSingleWindowDown picks.
findScreen :: ScreenNavConfig -> Direction2D -> WindowSet -> X (Maybe NavScreen)
findScreen cfg dir ws = case dir of
  U -> closestTo <$> getRefX ws <*> pure (filter (\s -> y s < curY && isUpper s) (W.screens ws))
  D | isUpper cur -> do
        refX <- getRefX ws
        let below = filter (\s -> y s > curY && not (isUpper s)) (W.screens ws)
        return $ case navSingleWindowDown cfg of
          Just p | isSingleWindow (W.workspace cur) -> listToMaybe (sortOn x (filter (p . rect) below))
          _ -> closestTo refX below
    | otherwise -> return Nothing
  L -> return (findLeftScreen ws)
  R -> return (findRightScreen ws)
  where
    cur = W.current ws
    curY = y cur
    rect = screenRect . W.screenDetail
    x = rect_x . rect
    y = rect_y . rect
    isUpper = navIsUpperScreen cfg . rect
    closestTo refX = listToMaybe . sortOn (\s -> let Rectangle sx _ sw _ = rect s
                                                 in abs (sx + fromIntegral sw `div` 2 - refX))

-- | Neighboring screen to the left: the closest right edge, then the most
-- vertical overlap.
findLeftScreen :: WindowSet -> Maybe NavScreen
findLeftScreen ws =
  let curScreen = W.current ws
      Rectangle cx cy _ ch = screenRect (W.screenDetail curScreen)
      cands = [ (s, sx + fromIntegral sw, intervalOverlap cy (cy + fromIntegral ch) sy (sy + fromIntegral sh))
              | s <- W.screens ws
              , W.screen s /= W.screen curScreen
              , let Rectangle sx sy sw sh = screenRect (W.screenDetail s)
              , sx + fromIntegral sw <= cx + 5
              ]
  in fmap (\(s, _, _) -> s) . listToMaybe $
       sortOn (\(_, rightEdge, ov) -> (Down rightEdge, Down ov)) cands

-- | Neighboring screen to the right: the closest left edge, then the most
-- vertical overlap.
findRightScreen :: WindowSet -> Maybe NavScreen
findRightScreen ws =
  let curScreen = W.current ws
      Rectangle cx cy cw ch = screenRect (W.screenDetail curScreen)
      cands = [ (s, sx, intervalOverlap cy (cy + fromIntegral ch) sy (sy + fromIntegral sh))
              | s <- W.screens ws
              , W.screen s /= W.screen curScreen
              , let Rectangle sx sy _ sh = screenRect (W.screenDetail s)
              , sx >= cx + fromIntegral cw - 5
              ]
  in fmap (\(s, _, _) -> s) . listToMaybe $
       sortOn (\(_, leftEdge, ov) -> (leftEdge, Down ov)) cands

-- | Overlap between two 1D intervals [a1, a2] and [b1, b2].
intervalOverlap :: (Ord a, Num a) => a -> a -> a -> a -> a
intervalOverlap a1 a2 b1 b2 = max 0 (min a2 b2 - max a1 b1)

-- | Reference X coordinate for directional jumping: the center of the
-- focused window, or of the current screen if there is none.
getRefX :: WindowSet -> X Position
getRefX ws = do
  mRect <- case W.peek ws of
    Nothing  -> return Nothing
    Just win -> withDisplay $ \dpy ->
      catchX (do (_, wx, _, w, _, bw, _) <- io $ getGeometry dpy win
                 return $ Just (wx, w + 2 * bw))
             (return Nothing)
  let Rectangle sx _ sw _ = screenRect (W.screenDetail (W.current ws))
      (x, w) = fromMaybe (sx, sw) mRect
  return (x + fromIntegral w `div` 2)

-- | Check if workspace has at most 1 window.
isSingleWindow :: W.Workspace i l a -> Bool
isSingleWindow wsp = case W.stack wsp of
  Nothing -> True
  Just st -> null (W.up st) && null (W.down st)

-- | Swap two windows across workspaces in WindowSet.
swapTwoWindows :: Window -> Window -> WindowSet -> WindowSet
swapTwoWindows w1 w2 winset =
  let swapW w | w == w1   = w2
              | w == w2   = w1
              | otherwise = w
      mapStack st = st { W.focus = swapW (W.focus st)
                       , W.up    = map swapW (W.up st)
                       , W.down  = map swapW (W.down st)
                       }
      mapWs wsp = wsp { W.stack = fmap mapStack (W.stack wsp) }
      mapScr scr = scr { W.workspace = mapWs (W.workspace scr) }
      swapFlt m =
        let m' = M.delete w1 (M.delete w2 m)
            m'' = maybe m' (\r -> M.insert w2 r m') (M.lookup w1 m)
        in maybe m'' (\r -> M.insert w1 r m'') (M.lookup w2 m)
  in winset { W.current  = mapScr (W.current winset)
            , W.visible  = map mapScr (W.visible winset)
            , W.floating = swapFlt (W.floating winset)
            }

-- | Undo a Navigation2D attempt that left the current workspace or moved its
-- focus. Nothing is refreshed when the attempt changed nothing.
restoreIfMoved :: WindowSet -> X ()
restoreIfMoved wsBefore = do
  wsAfter <- gets windowset
  when (W.currentTag wsAfter /= W.currentTag wsBefore || W.peek wsAfter /= W.peek wsBefore) $
    windows (const wsBefore)

-- | Select a screen (even if empty) and focus/warp pointer to it.
selectScreen :: ScreenNavConfig -> NavScreen -> X ()
selectScreen cfg sc = do
  curTag <- gets (W.currentTag . windowset)
  let targetTag = W.tag (W.workspace sc)
  when (targetTag /= curTag) $ do
    navBeforeScreenChange cfg
    windows (W.view targetTag)
    navFocusPointer cfg
    navAfterScreenChange cfg

-- | Move focus within the workspace if Navigation2D finds a window there,
-- otherwise select the adjacent screen.
focusDir :: Direction2D -> ScreenNavConfig -> X ()
focusDir dir cfg = do
  wsBefore <- gets windowset
  Nav2d.windowGo dir False
  wsAfter <- gets windowset
  if W.currentTag wsAfter == W.currentTag wsBefore && W.peek wsAfter /= W.peek wsBefore
    then navFocusPointer cfg
    else do
      restoreIfMoved wsBefore
      findScreen cfg dir wsBefore >>= maybe (navFocusPointer cfg) (selectScreen cfg)

-- | Swap within the workspace if Navigation2D changes it, otherwise swap with
-- the focused window of the adjacent screen, or move there if it is empty.
windowSwapDir :: Direction2D -> ScreenNavConfig -> X ()
windowSwapDir dir cfg = do
  wsBefore <- gets windowset
  let curTag = W.currentTag wsBefore
      stackOf = fmap (W.integrate' . W.stack) . find ((== curTag) . W.tag) . map W.workspace . W.screens
  Nav2d.windowSwap dir False
  wsAfter <- gets windowset
  if stackOf wsBefore /= stackOf wsAfter
    then navFocusPointer cfg
    else do
      restoreIfMoved wsBefore
      whenJust (W.peek wsBefore) $ \curWin ->
        findScreen cfg dir wsBefore >>= \target -> case target of
          Nothing -> navFocusPointer cfg
          Just sc -> do
            let t = W.tag (W.workspace sc)
            when (t /= curTag) $ do
              windows $ case W.stack (W.workspace sc) of
                Just st -> swapTwoWindows curWin (W.focus st)
                Nothing -> W.view t . W.shift t
              navFocusPointer cfg
              navAfterScreenChange cfg

-- | Swap the current workspace with the one on the adjacent screen and follow focus.
-- Shows the swapped workspace in dimmed colors on the origin screen, and the active workspace in bright colors on the target screen.
screenSwapDir :: Direction2D -> ScreenNavConfig -> X ()
screenSwapDir dir cfg = do
  ws <- gets windowset
  let curTag = W.currentTag ws
  findScreen cfg dir ws >>= \target -> case target of
    Nothing -> navFocusPointer cfg
    Just sc -> do
      let t = W.tag (W.workspace sc)
      when (t /= curTag) $ do
        windows (W.greedyView t)
        navBeforeScreenChange cfg
        windows (W.view curTag)
        navFocusPointer cfg
        navAfterScreenChange cfg

-- | Change the windowset with f, given the current and the adjacent screen's
-- workspace tags.
toScreenDir :: Direction2D -> (WorkspaceId -> WorkspaceId -> WindowSet -> WindowSet) -> ScreenNavConfig -> X ()
toScreenDir dir f cfg = do
  ws <- gets windowset
  let curTag = W.currentTag ws
  findScreen cfg dir ws >>= \target -> case target of
    Nothing -> navFocusPointer cfg
    Just sc -> do
      let t = W.tag (W.workspace sc)
      when (t /= curTag) $ do
        navBeforeScreenChange cfg
        windows (f curTag t)
        navFocusPointer cfg
        navAfterScreenChange cfg
