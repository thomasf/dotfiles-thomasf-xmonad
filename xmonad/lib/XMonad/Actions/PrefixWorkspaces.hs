-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.PrefixWorkspaces
-- Description :  Navigation and management of dot-separated hierarchical workspaces
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities for managing workspaces with prefixes (e.g. "proj.0", "proj.code",
-- "proj@worktree.code"). Supports cycling through workspaces with the same prefix,
-- creating new numbered sub-workspaces, and prompting for Git worktree workspaces.
-----------------------------------------------------------------------------

module XMonad.Actions.PrefixWorkspaces
    ( defaultWsSeparator
    , workspacePrefix
    , wsiWith
    , nextWsPrefixWith
    , prevWsPrefixWith
    , newPrefixWSWith
    , gotoPrefixWSWith
    , gotoBaseWSWith
    , worktreePromptWith
    , radialScreenRank
    , fillPrefixWSWith
    , fillPrefixWSWith'
    , removeEmptyHiddenWorkspacesExcept
    , removeEmptyHiddenWorkspacesExcept'
    ) where

import           Control.Monad (unless)
import           Data.List (delete, find, findIndex, nub, sort, sortBy, sortOn, isPrefixOf, (\\))
import           Data.Maybe (mapMaybe, isJust, fromMaybe)
import           Text.Read (readMaybe)
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Actions.CycleWS (WSType(..), Direction1D(..), findWorkspace, hiddenWS, wsTagGroup)
import           XMonad.Util.WorkspaceCompare (WorkspaceSort)
import           XMonad.Util.Run (runProcessWithInput)
import qualified XMonad.Prompt as XP
import           XMonad.Prompt.Subsequence (WsPrompt(..))

-- | Default workspace hierarchy separator.
defaultWsSeparator :: Char
defaultWsSeparator = '.'

-- | Extract the prefix of a workspace name up to the given separator.
workspacePrefix :: Char -> WorkspaceId -> String
workspacePrefix sep = takeWhile (/= sep)

-- | WSType matching hidden workspaces with the same prefix as the current workspace.
wsiWith :: Char -> WSType
wsiWith sep = hiddenWS :&: wsTagGroup sep

-- | Select next workspace with same prefix using the specified sort.
nextWsPrefixWith :: Char -> X WorkspaceSort -> X ()
nextWsPrefixWith sep sorter =
  windows . W.greedyView =<< findWorkspace sorter Next (wsiWith sep) 1

-- | Select previous workspace with same prefix using the specified sort.
prevWsPrefixWith :: Char -> X WorkspaceSort -> X ()
prevWsPrefixWith sep sorter =
  windows . W.greedyView =<< findWorkspace sorter Prev (wsiWith sep) 1

-- | View the workspace if it exists, otherwise call 'viewWs' to create and show it.
viewOrCreate :: (WorkspaceId -> X ()) -> WorkspaceId -> X ()
viewOrCreate viewWs t = do
  exists <- gets (W.tagMember t . windowset)
  if exists then windows (W.view t) else viewWs t

-- | Create a new prefixed sub-workspace with the lowest available numeric index.
-- If the workspace is new, 'viewWs' is called to create and show it.
newPrefixWSWith :: Char -> (WorkspaceId -> X ()) -> X ()
newPrefixWSWith sep viewWs = withWindowSet $ \w -> do
  let currentTagPrefix = workspacePrefix sep (W.currentTag w)
      prefixWithSep = currentTagPrefix ++ [sep]

      cws = map W.tag $ filter (\ws -> prefixWithSep `isPrefixOf` W.tag ws && isJust (W.stack ws)) (W.workspaces w)
      num = head $ [0..] \\ mapMaybe (readMaybe . drop (length prefixWithSep)) cws :: Integer
  viewOrCreate viewWs (prefixWithSep ++ show num)

-- | Switch to a sub-workspace with the given suffix under the current prefix.
gotoPrefixWSWith :: Char -> (WorkspaceId -> X ()) -> String -> X ()
gotoPrefixWSWith sep viewWs suffix = withWindowSet $ \w ->
  viewOrCreate viewWs (workspacePrefix sep (W.currentTag w) ++ [sep] ++ suffix)

-- | Switch to the base prefix workspace (without any suffix).
gotoBaseWSWith :: Char -> (WorkspaceId -> X ()) -> X ()
gotoBaseWSWith sep viewWs = withWindowSet $ \w ->
  viewOrCreate viewWs (workspacePrefix sep (W.currentTag w))

-- | Prompt for a worktree of the project and go to the same workspace (same suffix) in it.
-- Completes on "ws wt list" and existing workspaces of the project.
worktreePromptWith :: Char -> XP.XPConfig -> (WorkspaceId -> X ()) -> X ()
worktreePromptWith sep xpc viewWs = do
  w <- gets windowset
  let thisWS = W.currentTag w
      wss = W.workspaces w
      (prefix, suffix) = break (== sep) thisWS
      projectOf = takeWhile (/= '@') . workspacePrefix sep
      project = projectOf thisWS
      existing = [workspacePrefix sep (W.tag ws) | ws <- wss, projectOf (W.tag ws) == project]
  listed <- lines <$> runProcessWithInput "sh" ["-c", "exec ws wt list \"$1\" 2>/dev/null", "sh", thisWS] ""
  let prefixes = delete prefix $ nub (listed ++ sort existing)
      go input = do
        let p = workspacePrefix sep input
            target = (if '@' `elem` p || p == project then p else project ++ "@" ++ p) ++ suffix
        unless (null p || target == thisWS) $ viewOrCreate viewWs target
  XP.mkXPrompt (WsPrompt ("worktree" ++ suffix ++ ": ")) xpc (XP.mkComplFunFromList' xpc prefixes) go

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

-- | Fill all screens with workspaces sharing the prefix of target workspace 'target',
-- using automatic radial screen prioritization radiating outward from the desk center.
-- Empty hidden workspaces not in 'kept' are pruned both before and after the fill.
fillPrefixWSWith :: Char
                 -> [WorkspaceId]
                 -> (WorkspaceId -> X ())
                 -> WorkspaceId
                 -> X ()
fillPrefixWSWith sep kept addWs target = do
  physScreens <- withDisplay getCleanedScreenInfo
  wset <- gets windowset
  let allScreenRects = map (screenRect . W.screenDetail) (W.current wset : W.visible wset)
      rank = radialScreenRank physScreens allScreenRects
  fillPrefixWSWith' sep kept rank addWs target

-- | Fill all screens with workspaces sharing the prefix of target workspace 'target',
-- using a custom screen scoring function (lower score = higher priority).
-- Empty hidden workspaces not in 'kept' are pruned both before and after the fill.
fillPrefixWSWith' :: Char
                  -> [WorkspaceId]
                  -> (Rectangle -> Double)
                  -> (WorkspaceId -> X ())
                  -> WorkspaceId
                  -> X ()
fillPrefixWSWith' sep kept screenRank addWs target = do
  removeEmptyHiddenWorkspacesExcept kept
  wset <- gets windowset
  let p = workspacePrefix sep target
      allWs = W.workspaces wset
      allTags = map W.tag allWs
      anchor = if target `elem` allTags
                 then target
                 else if (p ++ [sep, '0']) `elem` allTags
                        then p ++ [sep, '0']
                        else target
  unless (anchor `elem` allTags) $ addWs anchor

  wset' <- gets windowset
  let curScreen = W.current wset'
      visScreens = W.visible wset'
      curSid = W.screen curScreen

      isStable sc = W.screen sc /= curSid
                 && W.tag (W.workspace sc) /= anchor
                 && workspacePrefix sep (W.tag (W.workspace sc)) == p

      stableScreens = filter isStable visScreens
      stableTags = map (W.tag . W.workspace) stableScreens

      targetScreens = filter (not . isStable) visScreens

      screenRectOf sc = screenRect (W.screenDetail sc)
      compareScreens a b =
        let ra = screenRectOf a
            rb = screenRectOf b
            scoreA = screenRank ra
            scoreB = screenRank rb
        in if abs (scoreA - scoreB) < 100
             then compare (rect_x ra) (rect_x rb)
             else compare scoreA scoreB
      sortedTargetScreens = sortBy compareScreens targetScreens
      neededCount = length sortedTargetScreens

      isCandidate w =
        let tag = W.tag w
        in workspacePrefix sep tag == p
           && tag /= anchor
           && tag `notElem` stableTags
           && tag /= "NSP"
      candidateWs = filter isCandidate (W.workspaces wset')

      compareSuffix w1 w2 =
        let t1 = W.tag w1
            t2 = W.tag w2
            s1 = drop (length p) t1
            s2 = drop (length p) t2
            num1 = readMaybe (dropWhile (== sep) s1) :: Maybe Integer
            num2 = readMaybe (dropWhile (== sep) s2) :: Maybe Integer
        in case (s1, s2) of
             ("", "") -> EQ
             ("", _)  -> LT
             (_, "")  -> GT
             _ -> case (num1, num2) of
                    (Just n1, Just n2) -> compare n1 n2
                    (Just _, Nothing)  -> LT
                    (Nothing, Just _)  -> GT
                    (Nothing, Nothing) -> compare s1 s2

      compareCandidates w1 w2 =
        let occ1 = isJust (W.stack w1)
            occ2 = isJust (W.stack w2)
        in compare (not occ1) (not occ2) <> compareSuffix w1 w2

      sortedCandidates = map W.tag (sortBy compareCandidates candidateWs)
      availableCount = length sortedCandidates
      missingCount = max 0 (neededCount - availableCount)

  newTags <- if missingCount > 0
    then do
      let prefixWithSep = p ++ [sep]
          allTagsOfP = [W.tag w | w <- W.workspaces wset', workspacePrefix sep (W.tag w) == p]
          existingNums = mapMaybe (readMaybe . drop (length prefixWithSep)) allTagsOfP :: [Integer]
          unusedNums = [0..] \\ existingNums
          generated = [prefixWithSep ++ show n | n <- take missingCount unusedNums]
      mapM_ addWs generated
      return generated
    else return []

  let allAssignedTags = sortedCandidates ++ newTags
      screenAssignments = zip (map W.screen sortedTargetScreens) allAssignedTags

  windows $ \s ->
    let allWorkspaces = W.workspace (W.current s) : map W.workspace (W.visible s) ++ W.hidden s
        findWs t = fromMaybe (W.workspace (W.current s)) (find ((== t) . W.tag) allWorkspaces)

        curWs = findWs anchor
        newVis = [ if isStable sc
                     then sc
                     else case lookup (W.screen sc) screenAssignments of
                            Just t  -> sc { W.workspace = findWs t }
                            Nothing -> sc
                 | sc <- W.visible s ]
        newCur = (W.current s) { W.workspace = curWs }
        assignedSet = map (W.tag . W.workspace) (newCur : newVis)
        rawHidden = filter (\w -> W.tag w `notElem` assignedSet) allWorkspaces
        newHidden = filter (\w -> isJust (W.stack w) || W.tag w `elem` kept) rawHidden
    in s { W.current = newCur
         , W.visible = newVis
         , W.hidden  = newHidden
         }
