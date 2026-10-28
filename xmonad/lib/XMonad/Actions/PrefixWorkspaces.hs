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
    ) where

import           Control.Monad (unless)
import           Data.List (delete, nub, sort, isPrefixOf, (\\))
import           Data.Maybe (mapMaybe, isJust)
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
