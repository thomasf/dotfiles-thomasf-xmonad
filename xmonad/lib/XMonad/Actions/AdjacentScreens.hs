{-# LANGUAGE ScopedTypeVariables #-}
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
    , selectScreen
    , findUpperScreen
    , findUpperScreenWith
    , findLowerScreen
    , findLowerScreenWith
    , findLeftScreen
    , findRightScreen
    , isUpperScreen
    , isSingleWindowWorkspace
    , swapTwoWindows
    , intervalOverlap
    , getRefX
    ) where

import           Control.Monad (when)
import           Data.List (find, sortOn)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
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
  , navIsUpperScreen      :: ScreenDetail -> Bool  -- ^ Predicate identifying upper row screens
  }

-- | Default navigation configuration.
defaultScreenNavConfig :: ScreenNavConfig
defaultScreenNavConfig = ScreenNavConfig
  { navBeforeScreenChange = return ()
  , navAfterScreenChange  = return ()
  , navFocusPointer       = return ()
  , navIsUpperScreen      = isUpperScreen
  }

-- | Default check if a screen is on the upper row (y < 1000).
isUpperScreen :: ScreenDetail -> Bool
isUpperScreen sd =
  let Rectangle _ y _ _ = screenRect sd
  in y < 1000

-- | Overlap between two 1D intervals [a1, a2] and [b1, b2].
intervalOverlap :: (Ord a, Num a) => a -> a -> a -> a -> a
intervalOverlap a1 a2 b1 b2 = max 0 (min a2 b2 - max a1 b1)

-- | Select a screen (even if empty) and focus/warp pointer to it.
selectScreen :: ScreenNavConfig -> W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail -> X ()
selectScreen cfg sc = do
  curTag <- gets (W.tag . W.workspace . W.current . windowset)
  let targetTag = W.tag (W.workspace sc)
  when (targetTag /= curTag) $ do
    navBeforeScreenChange cfg
    windows (W.view targetTag)
    navFocusPointer cfg
    navAfterScreenChange cfg

-- | Find upper row screen (if any), preferring the one horizontally closest to reference x.
findUpperScreen :: WindowSet -> Position -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findUpperScreen = findUpperScreenWith isUpperScreen

-- | Find upper row screen using custom predicate.
findUpperScreenWith :: (ScreenDetail -> Bool) -> WindowSet -> Position -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findUpperScreenWith isUpper ws refX =
  let curY = rect_y (screenRect (W.screenDetail (W.current ws)))
      upperScreens = filter (\s -> let r = screenRect (W.screenDetail s) in rect_y r < curY && isUpper (W.screenDetail s)) (W.screens ws)
      dist s = let Rectangle sx _ sw _ = screenRect (W.screenDetail s)
                   cx = sx + fromIntegral sw `div` 2
               in abs (cx - refX)
  in listToMaybe $ sortOn dist upperScreens

-- | Find target lower screen from the upper row:
-- If singleWindow is True, target the leftmost pane of the center screen (rect_x == 3840 && rect_y >= 1000).
-- Otherwise, target the nearest pane below horizontally to refX.
findLowerScreen :: WindowSet -> Position -> Bool -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findLowerScreen = findLowerScreenWith isUpperScreen

-- | Find target lower screen from the upper row using custom predicate.
findLowerScreenWith :: (ScreenDetail -> Bool) -> WindowSet -> Position -> Bool -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findLowerScreenWith isUpper ws refX singleWindow =
  let curY = rect_y (screenRect (W.screenDetail (W.current ws)))
      belowScreens = filter (\s -> rect_y (screenRect (W.screenDetail s)) > curY && not (isUpper (W.screenDetail s))) (W.screens ws)
  in if singleWindow
       then case find (\s -> let Rectangle sx sy _ _ = screenRect (W.screenDetail s) in sx == 3840 && sy >= 1000) belowScreens of
              Just sc -> Just sc
              Nothing -> listToMaybe $ sortOn (\s -> rect_x (screenRect (W.screenDetail s))) [s | s <- belowScreens, rect_x (screenRect (W.screenDetail s)) >= 3840]
       else let dist s = let Rectangle sx _ sw _ = screenRect (W.screenDetail s)
                             cx = sx + fromIntegral sw `div` 2
                         in abs (cx - refX)
            in listToMaybe $ sortOn dist belowScreens

-- | Find neighboring screen to the left.
findLeftScreen :: WindowSet -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findLeftScreen ws =
  let curScreen = W.current ws
      Rectangle cx cy cw ch = screenRect (W.screenDetail curScreen)
      cands = [ (s, sx + fromIntegral sw, intervalOverlap cy (cy + fromIntegral ch) sy (sy + fromIntegral sh))
              | s <- W.screens ws
              , W.screen s /= W.screen curScreen
              , let Rectangle sx sy sw sh = screenRect (W.screenDetail s)
              , sx + fromIntegral sw <= cx + 5
              ]
  in fmap (\(s, _, _) -> s) . listToMaybe $
       sortOn (\(_, rightEdge, ov) -> (Down rightEdge, Down ov)) cands

-- | Find neighboring screen to the right.
findRightScreen :: WindowSet -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
findRightScreen ws =
  let curScreen = W.current ws
      Rectangle cx cy cw ch = screenRect (W.screenDetail curScreen)
      cands = [ (s, sx, intervalOverlap cy (cy + fromIntegral ch) sy (sy + fromIntegral sh))
              | s <- W.screens ws
              , W.screen s /= W.screen curScreen
              , let Rectangle sx sy sw sh = screenRect (W.screenDetail s)
              , sx >= cx + fromIntegral cw - 5
              ]
  in fmap (\(s, _, _) -> s) . listToMaybe $
       sortOn (\(_, leftEdge, ov) -> (leftEdge, Down ov)) cands

-- | Get reference X coordinate for directional jumping.
getRefX :: WindowSet -> X Position
getRefX ws = case W.peek ws of
  Just win -> do
    mRect <- withDisplay $ \dpy ->
      catchX (do (_, x, _, w, _, bw, _) <- io $ getGeometry dpy win
                 return $ Just $ Rectangle x 0 (w + 2 * bw) 0)
             (return Nothing)
    case mRect of
      Just (Rectangle x _ w _) -> return (x + fromIntegral w `div` 2)
      Nothing -> let Rectangle sx _ sw _ = screenRect (W.screenDetail (W.current ws))
                 in return (sx + fromIntegral sw `div` 2)
  Nothing -> let Rectangle sx _ sw _ = screenRect (W.screenDetail (W.current ws))
             in return (sx + fromIntegral sw `div` 2)

-- | Check if workspace has at most 1 window.
isSingleWindowWorkspace :: W.Workspace i l a -> Bool
isSingleWindowWorkspace wsp = case W.stack wsp of
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
        let r1 = M.lookup w1 m
            r2 = M.lookup w2 m
            m' = M.delete w1 (M.delete w2 m)
            m'' = case r1 of
                    Just r  -> M.insert w2 r m'
                    Nothing -> m'
        in case r2 of
             Just r  -> M.insert w1 r m''
             Nothing -> m''
  in winset { W.current  = mapScr (W.current winset)
            , W.visible  = map mapScr (W.visible winset)
            , W.floating = swapFlt (W.floating winset)
            }

-- | Move focus up: within workspace first, then jump up wherever there is something on upper row.
focusUp :: ScreenNavConfig -> X ()
focusUp cfg = do
  wsBefore <- gets windowset
  let curTagBefore = W.tag (W.workspace (W.current wsBefore))
      curWinBefore = W.peek wsBefore
  Nav2d.windowGo U False
  wsAfter <- gets windowset
  let curTagAfter = W.tag (W.workspace (W.current wsAfter))
      curWinAfter = W.peek wsAfter
  if curTagAfter == curTagBefore && curWinAfter /= curWinBefore
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      refX <- getRefX wsBefore
      case findUpperScreenWith (navIsUpperScreen cfg) wsBefore refX of
        Just upperSc -> selectScreen cfg upperSc
        Nothing      -> navFocusPointer cfg

-- | Move focus down: within workspace first; from upper row, jump to nearest pane below
-- (or leftmost pane of center screen if single window on top row). Never skips empty screens.
focusDown :: ScreenNavConfig -> X ()
focusDown cfg = do
  wsBefore <- gets windowset
  let curScreenBefore = W.current wsBefore
      curTagBefore    = W.tag (W.workspace curScreenBefore)
      curWinBefore    = W.peek wsBefore
  Nav2d.windowGo D False
  wsAfter <- gets windowset
  let curTagAfter = W.tag (W.workspace (W.current wsAfter))
      curWinAfter = W.peek wsAfter
  if curTagAfter == curTagBefore && curWinAfter /= curWinBefore
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      if navIsUpperScreen cfg (W.screenDetail curScreenBefore)
        then do
          refX <- getRefX wsBefore
          let singleWin = isSingleWindowWorkspace (W.workspace curScreenBefore)
          case findLowerScreenWith (navIsUpperScreen cfg) wsBefore refX singleWin of
            Just lowerSc -> selectScreen cfg lowerSc
            Nothing      -> navFocusPointer cfg
        else navFocusPointer cfg

-- | Move focus left: within workspace first, then jump to neighboring screen to the left. Never skips empty screens.
focusLeft :: ScreenNavConfig -> X ()
focusLeft cfg = do
  wsBefore <- gets windowset
  let curTagBefore = W.tag (W.workspace (W.current wsBefore))
      curWinBefore = W.peek wsBefore
  Nav2d.windowGo L False
  wsAfter <- gets windowset
  let curTagAfter = W.tag (W.workspace (W.current wsAfter))
      curWinAfter = W.peek wsAfter
  if curTagAfter == curTagBefore && curWinAfter /= curWinBefore
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      case findLeftScreen wsBefore of
        Just leftSc -> selectScreen cfg leftSc
        Nothing     -> navFocusPointer cfg

-- | Move focus right: within workspace first, then jump to neighboring screen to the right. Never skips empty screens.
focusRight :: ScreenNavConfig -> X ()
focusRight cfg = do
  wsBefore <- gets windowset
  let curTagBefore = W.tag (W.workspace (W.current wsBefore))
      curWinBefore = W.peek wsBefore
  Nav2d.windowGo R False
  wsAfter <- gets windowset
  let curTagAfter = W.tag (W.workspace (W.current wsAfter))
      curWinAfter = W.peek wsAfter
  if curTagAfter == curTagBefore && curWinAfter /= curWinBefore
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      case findRightScreen wsBefore of
        Just rightSc -> selectScreen cfg rightSc
        Nothing      -> navFocusPointer cfg

-- | Swap window up: within workspace first, then across to upper screen.
windowSwapUp :: ScreenNavConfig -> X ()
windowSwapUp cfg = do
  wsBefore <- gets windowset
  let curScreenBefore = W.current wsBefore
      curTagBefore    = W.tag (W.workspace curScreenBefore)
      stackBefore     = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsBefore))
  Nav2d.windowSwap U False
  wsAfter <- gets windowset
  let stackAfter = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsAfter))
  if stackBefore /= stackAfter
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      case W.peek wsBefore of
        Nothing -> return ()
        Just curWin -> do
          refX <- getRefX wsBefore
          case findUpperScreenWith (navIsUpperScreen cfg) wsBefore refX of
            Just upperSc -> do
              let upperTag = W.tag (W.workspace upperSc)
              when (upperTag /= curTagBefore) $ do
                case fmap W.focus (W.stack (W.workspace upperSc)) of
                  Just targetWin -> do
                    windows (swapTwoWindows curWin targetWin)
                    navFocusPointer cfg
                    navAfterScreenChange cfg
                  Nothing -> do
                    windows (W.view upperTag . W.shift upperTag)
                    navFocusPointer cfg
                    navAfterScreenChange cfg
            Nothing -> navFocusPointer cfg

-- | Swap window down: within workspace first; from upper row, across to target lower screen.
windowSwapDown :: ScreenNavConfig -> X ()
windowSwapDown cfg = do
  wsBefore <- gets windowset
  let curScreenBefore = W.current wsBefore
      curTagBefore    = W.tag (W.workspace curScreenBefore)
      stackBefore     = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsBefore))
  Nav2d.windowSwap D False
  wsAfter <- gets windowset
  let stackAfter = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsAfter))
  if stackBefore /= stackAfter
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      if navIsUpperScreen cfg (W.screenDetail curScreenBefore)
        then case W.peek wsBefore of
          Nothing -> return ()
          Just curWin -> do
            refX <- getRefX wsBefore
            let singleWin = isSingleWindowWorkspace (W.workspace curScreenBefore)
            case findLowerScreenWith (navIsUpperScreen cfg) wsBefore refX singleWin of
              Just lowerSc -> do
                let lowerTag = W.tag (W.workspace lowerSc)
                when (lowerTag /= curTagBefore) $ do
                  case fmap W.focus (W.stack (W.workspace lowerSc)) of
                    Just targetWin -> do
                      windows (swapTwoWindows curWin targetWin)
                      navFocusPointer cfg
                      navAfterScreenChange cfg
                    Nothing -> do
                      windows (W.view lowerTag . W.shift lowerTag)
                      navFocusPointer cfg
                      navAfterScreenChange cfg
              Nothing -> navFocusPointer cfg
        else navFocusPointer cfg

-- | Swap window left: within workspace first, then across to left screen.
windowSwapLeft :: ScreenNavConfig -> X ()
windowSwapLeft cfg = do
  wsBefore <- gets windowset
  let curScreenBefore = W.current wsBefore
      curTagBefore    = W.tag (W.workspace curScreenBefore)
      stackBefore     = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsBefore))
  Nav2d.windowSwap L False
  wsAfter <- gets windowset
  let stackAfter = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsAfter))
  if stackBefore /= stackAfter
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      case W.peek wsBefore of
        Nothing -> return ()
        Just curWin -> case findLeftScreen wsBefore of
          Just leftSc -> do
            let leftTag = W.tag (W.workspace leftSc)
            when (leftTag /= curTagBefore) $ do
              case fmap W.focus (W.stack (W.workspace leftSc)) of
                Just targetWin -> do
                  windows (swapTwoWindows curWin targetWin)
                  navFocusPointer cfg
                  navAfterScreenChange cfg
                Nothing -> do
                  windows (W.view leftTag . W.shift leftTag)
                  navFocusPointer cfg
                  navAfterScreenChange cfg
          Nothing -> navFocusPointer cfg

-- | Swap window right: within workspace first, then across to right screen.
windowSwapRight :: ScreenNavConfig -> X ()
windowSwapRight cfg = do
  wsBefore <- gets windowset
  let curScreenBefore = W.current wsBefore
      curTagBefore    = W.tag (W.workspace curScreenBefore)
      stackBefore     = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsBefore))
  Nav2d.windowSwap R False
  wsAfter <- gets windowset
  let stackAfter = fmap (W.integrate' . W.stack . W.workspace) (find ((== curTagBefore) . W.tag . W.workspace) (W.screens wsAfter))
  if stackBefore /= stackAfter
    then navFocusPointer cfg
    else do
      windows (const wsBefore)
      case W.peek wsBefore of
        Nothing -> return ()
        Just curWin -> case findRightScreen wsBefore of
          Just rightSc -> do
            let rightTag = W.tag (W.workspace rightSc)
            when (rightTag /= curTagBefore) $ do
              case fmap W.focus (W.stack (W.workspace rightSc)) of
                Just targetWin -> do
                  windows (swapTwoWindows curWin targetWin)
                  navFocusPointer cfg
                  navAfterScreenChange cfg
                Nothing -> do
                  windows (W.view rightTag . W.shift rightTag)
                  navFocusPointer cfg
                  navAfterScreenChange cfg
          Nothing -> navFocusPointer cfg

-- | Swap screen above: swap workspace with upper row screen.
screenSwapUp :: ScreenNavConfig -> X ()
screenSwapUp cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  refX <- getRefX ws
  case findUpperScreenWith (navIsUpperScreen cfg) ws refX of
    Just upperSc -> do
      let upperTag = W.tag (W.workspace upperSc)
      when (upperTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view curTag . W.greedyView upperTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg

-- | Swap screen below: from upper row, swap workspace with target lower screen.
screenSwapDown :: ScreenNavConfig -> X ()
screenSwapDown cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  if navIsUpperScreen cfg (W.screenDetail curScreen)
    then do
      refX <- getRefX ws
      let singleWin = isSingleWindowWorkspace (W.workspace curScreen)
      case findLowerScreenWith (navIsUpperScreen cfg) ws refX singleWin of
        Just lowerSc -> do
          let lowerTag = W.tag (W.workspace lowerSc)
          when (lowerTag /= curTag) $ do
            navBeforeScreenChange cfg
            windows (W.view curTag . W.greedyView lowerTag)
            navFocusPointer cfg
            navAfterScreenChange cfg
        Nothing -> navFocusPointer cfg
    else navFocusPointer cfg

-- | Swap screen left: swap workspace with left screen.
screenSwapLeft :: ScreenNavConfig -> X ()
screenSwapLeft cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  case findLeftScreen ws of
    Just leftSc -> do
      let leftTag = W.tag (W.workspace leftSc)
      when (leftTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view curTag . W.greedyView leftTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg

-- | Swap screen right: swap workspace with right screen.
screenSwapRight :: ScreenNavConfig -> X ()
screenSwapRight cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  case findRightScreen ws of
    Just rightSc -> do
      let rightTag = W.tag (W.workspace rightSc)
      when (rightTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view curTag . W.greedyView rightTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg

-- | Drag window to screen above: shift focused window to upper row screen and follow focus.
windowToScreenUp :: ScreenNavConfig -> X ()
windowToScreenUp cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  refX <- getRefX ws
  case findUpperScreenWith (navIsUpperScreen cfg) ws refX of
    Just upperSc -> do
      let upperTag = W.tag (W.workspace upperSc)
      when (upperTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view upperTag . W.shift upperTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg

-- | Drag window to screen below: from upper row, shift focused window to target lower screen and follow focus.
windowToScreenDown :: ScreenNavConfig -> X ()
windowToScreenDown cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  if navIsUpperScreen cfg (W.screenDetail curScreen)
    then do
      refX <- getRefX ws
      let singleWin = isSingleWindowWorkspace (W.workspace curScreen)
      case findLowerScreenWith (navIsUpperScreen cfg) ws refX singleWin of
        Just lowerSc -> do
          let lowerTag = W.tag (W.workspace lowerSc)
          when (lowerTag /= curTag) $ do
            navBeforeScreenChange cfg
            windows (W.view lowerTag . W.shift lowerTag)
            navFocusPointer cfg
            navAfterScreenChange cfg
        Nothing -> navFocusPointer cfg
    else navFocusPointer cfg

-- | Drag window to screen left: shift focused window to left screen and follow focus.
windowToScreenLeft :: ScreenNavConfig -> X ()
windowToScreenLeft cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  case findLeftScreen ws of
    Just leftSc -> do
      let leftTag = W.tag (W.workspace leftSc)
      when (leftTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view leftTag . W.shift leftTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg

-- | Drag window to screen right: shift focused window to right screen and follow focus.
windowToScreenRight :: ScreenNavConfig -> X ()
windowToScreenRight cfg = do
  ws <- gets windowset
  let curScreen = W.current ws
      curTag    = W.tag (W.workspace curScreen)
  case findRightScreen ws of
    Just rightSc -> do
      let rightTag = W.tag (W.workspace rightSc)
      when (rightTag /= curTag) $ do
        navBeforeScreenChange cfg
        windows (W.view rightTag . W.shift rightTag)
        navFocusPointer cfg
        navAfterScreenChange cfg
    Nothing -> navFocusPointer cfg
