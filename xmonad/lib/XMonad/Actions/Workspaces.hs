-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Workspaces
-- Description :  Multi-screen workspace assignment, screen prioritization and cleanup
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities for assigning arbitrary lists of workspaces across available
-- screens based on display geometry and radial distance from the desk center,
-- as well as pruning unoccupied hidden workspaces.
-----------------------------------------------------------------------------

module XMonad.Actions.Workspaces
    ( radialScreenRank
    , removeEmptyHiddenWorkspacesExcept
    , removeEmptyHiddenWorkspacesExcept'
    , viewWorkspacesOnScreens
    ) where

import           Control.Monad (forM_, unless)
import           Data.List (find, sortBy, sortOn)
import           Data.Maybe (fromMaybe, isJust)
import           XMonad
import qualified XMonad.StackSet as W

-- | Compute the ranking score for a screen rectangle based on radial distance
-- from the center of the main display area.
-- Lower score = filled first.
-- Screens on upper rows receive a high score (+100000) so they are filled last.
radialScreenRank :: [Rectangle] -> [Rectangle] -> Rectangle -> Double
radialScreenRank physScreens allRects r
  | isAbove r = 100000.0 + abs (cx r - deskCenterX)
  | otherwise = abs (cx r - deskCenterX)
  where
    cy rect = fromIntegral (rect_y rect) + fromIntegral (rect_height rect) / 2 :: Double
    cx rect = fromIntegral (rect_x rect) + fromIntegral (rect_width rect) / 2 :: Double

    refRects = if null physScreens then allRects else physScreens
    cys = map cy refRects
    minCY = if null cys then 0 else minimum cys
    maxCY = if null cys then 0 else maximum cys
    hasMultipleRows = maxCY - minCY > 200
    midY = (minCY + maxCY) / 2
    isAbove rect = hasMultipleRows && cy rect < midY

    lowerPhys = filter (not . isAbove) refRects
    sortedPhys = sortOn rect_x lowerPhys
    numPhys = length sortedPhys
    centerMonitor = if numPhys == 0
                      then Rectangle 0 0 1920 1080
                      else sortedPhys !! (numPhys `div` 2)
    deskCenterX = fromIntegral (rect_x centerMonitor) + fromIntegral (rect_width centerMonitor) / 2 :: Double

-- | Remove all hidden workspaces that are empty (contain no windows),
-- except those whose tags are in the given list.
removeEmptyHiddenWorkspacesExcept :: [WorkspaceId] -> X ()
removeEmptyHiddenWorkspacesExcept kept = windows (removeEmptyHiddenWorkspacesExcept' kept)

-- | Pure transformation to remove empty hidden workspaces except those in the given list.
removeEmptyHiddenWorkspacesExcept' :: [WorkspaceId] -> WindowSet -> WindowSet
removeEmptyHiddenWorkspacesExcept' kept s =
  s { W.hidden = filter (\w -> isJust (W.stack w) || W.tag w `elem` kept) (W.hidden s) }

-- | View a list of workspaces across screens.
-- The currently focused screen gets the first workspace.
-- Subsequent workspaces are assigned to other screens sorted radially outward from the desk center.
-- Empty hidden workspaces not in 'kept' are pruned after switching.
viewWorkspacesOnScreens :: [WorkspaceId]
                        -> (WorkspaceId -> X ())
                        -> [WorkspaceId]
                        -> X ()
viewWorkspacesOnScreens _ _ [] = return ()
viewWorkspacesOnScreens kept addWs targets@(primary : _) = do
  forM_ targets $ \t -> do
    exists <- gets (W.tagMember t . windowset)
    unless exists $ addWs t

  physScreens <- withDisplay getCleanedScreenInfo
  wset <- gets windowset
  let allScreenRects = map (screenRect . W.screenDetail) (W.current wset : W.visible wset)
      rank = radialScreenRank physScreens allScreenRects

      curScreen = W.current wset
      visScreens = W.visible wset

      screenRectOf sc = screenRect (W.screenDetail sc)
      compareScreens a b =
        let ra = screenRectOf a
            rb = screenRectOf b
            scoreA = rank ra
            scoreB = rank rb
        in if abs (scoreA - scoreB) < 100
             then compare (rect_x ra) (rect_x rb)
             else compare scoreA scoreB
      sortedVisScreens = sortBy compareScreens visScreens

      screensInOrder = W.screen curScreen : map W.screen sortedVisScreens
      assignments = zip screensInOrder targets

  windows $ \s ->
    let viewSc sc ws = case find ((== sc) . W.screen) (W.current ws : W.visible ws) of
                         Just scr -> W.view (W.tag (W.workspace scr)) ws
                         Nothing  -> ws
        step (sc, t) ws = W.greedyView t (viewSc sc ws)
        s1 = foldl (flip step) s assignments
        sFinal = W.view primary s1
    in removeEmptyHiddenWorkspacesExcept' kept sFinal
