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
import           Text.Printf (printf)
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Actions.CycleWS (WSType(..), Direction1D(..), findWorkspace)
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
wsiWith sep = WSIs wsPred
  where
    wsPredHidden = do
      hs <- gets (map W.tag . W.hidden . windowset)
      return (\w -> W.tag w `elem` hs)

    wsPredGroup = do
      cur <- (groupName . W.workspace . W.current) `fmap` gets windowset
      return $ (cur ==) . groupName
      where groupName = workspacePrefix sep . W.tag

    wsPred = do
      tg <- wsPredGroup
      hi <- wsPredHidden
      return (\w -> tg w && hi w)

-- | Select next workspace with same prefix using the specified sort.
nextWsPrefixWith :: Char -> X WorkspaceSort -> X ()
nextWsPrefixWith sep sorter =
  windows . W.greedyView =<< findWorkspace sorter Next (wsiWith sep) 1

-- | Select previous workspace with same prefix using the specified sort.
prevWsPrefixWith :: Char -> X WorkspaceSort -> X ()
prevWsPrefixWith sep sorter =
  windows . W.greedyView =<< findWorkspace sorter Prev (wsiWith sep) 1

-- | Create a new prefixed sub-workspace with the lowest available numeric index.
-- If the workspace is new, 'viewWs' is called to initialize it.
newPrefixWSWith :: Char -> (WorkspaceId -> X ()) -> X ()
newPrefixWSWith sep viewWs = withWindowSet $ \w -> do
  thisWS <- gets (W.currentTag . windowset)
  let wss = W.workspaces w
      currentTagPrefix = workspacePrefix sep thisWS
      prefixWithSep = currentTagPrefix ++ [sep]

      cws = map W.tag $ filter (\ws -> prefixWithSep `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0..] \\ mapMaybe (readMaybe . drop (length prefixWithSep)) cws :: Integer
      new = printf "%s%c%i" currentTagPrefix sep num
  unless (new `elem` map W.tag wss) $ viewWs new
  windows $ W.view new

-- | Switch to a sub-workspace with the given suffix under the current prefix.
gotoPrefixWSWith :: Char -> (WorkspaceId -> X ()) -> String -> X ()
gotoPrefixWSWith sep viewWs suffix = withWindowSet $ \w -> do
  thisWS <- gets (W.currentTag . windowset)
  let wss = W.workspaces w
      currentTagPrefix = workspacePrefix sep thisWS
      new = currentTagPrefix ++ [sep] ++ suffix
  unless (new `elem` map W.tag wss) $ viewWs new
  windows $ W.view new

-- | Switch to the base prefix workspace (without any suffix).
gotoBaseWSWith :: Char -> (WorkspaceId -> X ()) -> X ()
gotoBaseWSWith sep viewWs = withWindowSet $ \w -> do
  thisWS <- gets (W.currentTag . windowset)
  let wss = W.workspaces w
      new = workspacePrefix sep thisWS
  unless (new `elem` map W.tag wss) $ viewWs new
  windows $ W.view new

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
        unless (null p || target == thisWS) $ do
          unless (target `elem` map W.tag wss) $ viewWs target
          windows $ W.view target
  XP.mkXPrompt (WsPrompt ("worktree" ++ suffix ++ ": ")) xpc (XP.mkComplFunFromList' xpc prefixes) go
