-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceIPC
-- Description :  ServerMode-style event hooks for external workspace/window switching via client messages
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Listens for ClientMessageEvents with atom "WS_GO" (to switch workspaces by name),
-- "WS_SHIFT" (to move focused window to workspace by name), "WS_RENAME" (to rename
-- the current workspace), or "WS_FOCUS" (to focus windows by XID), allowing external
-- CLI tools (e.g. "ws go", "ws shift", "ws rename", "ws window") to interact with XMonad.
-----------------------------------------------------------------------------

module XMonad.Hooks.WorkspaceIPC
    ( wsAtomServerModeHook'
    , wsGoServerModeHook'
    , wsGoServerModeHook
    , wsFocusServerModeHook'
    , wsFocusServerModeHook
    , wsShiftServerModeHook'
    , wsShiftServerModeHook
    , wsRenameServerModeHook'
    , wsRenameServerModeHook
    ) where

import           Control.Monad (when, forM_, mfilter)
import           Data.Monoid (All(..))
import           XMonad
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.StackSet as W

-- | Listens for ClientMessageEvent with the given atom name, extracts the
-- payload atom as a string, and runs the given workspace action.
wsAtomServerModeHook' :: String -> (WorkspaceId -> X ()) -> Event -> X All
wsAtomServerModeHook' atomName handleWs ClientMessageEvent {ev_message_type = mt, ev_data = dt} = do
    atom <- getAtom atomName
    when (mt == atom) $ do
      d <- asks display
      forM_ (take 1 dt) $ \a -> do
        name <- io $ getAtomName d (fromIntegral a)
        forM_ (mfilter (not . null) name) handleWs
    return (All True)
wsAtomServerModeHook' _ _ _ = return (All True)

-- | Listens for "WS_GO" ClientMessageEvent, extracts the atom name, and runs
-- the given workspace action.
wsGoServerModeHook' :: (WorkspaceId -> X ()) -> Event -> X All
wsGoServerModeHook' = wsAtomServerModeHook' "WS_GO"

-- | Default 'wsGoServerModeHook' which simply views the target workspace.
wsGoServerModeHook :: Event -> X All
wsGoServerModeHook = wsGoServerModeHook' (windows . W.view)

-- | Listens for "WS_SHIFT" ClientMessageEvent, extracts the atom name, and runs
-- the given workspace action.
wsShiftServerModeHook' :: (WorkspaceId -> X ()) -> Event -> X All
wsShiftServerModeHook' = wsAtomServerModeHook' "WS_SHIFT"

-- | Default 'wsShiftServerModeHook' which simply shifts the focused window to the target workspace.
wsShiftServerModeHook :: Event -> X All
wsShiftServerModeHook = wsShiftServerModeHook' (windows . W.shift)

-- | Listens for "WS_RENAME" ClientMessageEvent, extracts the atom name, and runs
-- the given workspace action.
wsRenameServerModeHook' :: (WorkspaceId -> X ()) -> Event -> X All
wsRenameServerModeHook' = wsAtomServerModeHook' "WS_RENAME"

-- | Default 'wsRenameServerModeHook' which simply renames the current workspace.
wsRenameServerModeHook :: Event -> X All
wsRenameServerModeHook = wsRenameServerModeHook' DW.renameWorkspaceByName

-- | Listens for "WS_FOCUS" ClientMessageEvent, extracts the window ID,
-- verifies it is managed, and runs the given window action.
wsFocusServerModeHook' :: (Window -> X ()) -> Event -> X All
wsFocusServerModeHook' handleWin ClientMessageEvent {ev_message_type = mt, ev_data = dt} = do
    wsFocus <- getAtom "WS_FOCUS"
    when (mt == wsFocus) $ forM_ (take 1 dt) $ \a -> do
      let w = fromIntegral a
      managed <- gets (W.member w . windowset)
      when managed $ handleWin w
    return (All True)
wsFocusServerModeHook' _ _ = return (All True)

-- | Default 'wsFocusServerModeHook' which simply focuses the target window.
wsFocusServerModeHook :: Event -> X All
wsFocusServerModeHook = wsFocusServerModeHook' (windows . W.focusWindow)
