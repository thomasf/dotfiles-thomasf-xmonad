-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceIPC
-- Description :  ServerMode-style event hooks for external workspace/window switching via client messages
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Listens for ClientMessageEvents with atom "WS_GO" (to switch workspaces by name)
-- or "WS_FOCUS" (to focus windows by XID), allowing external CLI tools (e.g. "ws go",
-- "ws window") to interact with XMonad.
-----------------------------------------------------------------------------

module XMonad.Hooks.WorkspaceIPC
    ( wsGoServerModeHook'
    , wsGoServerModeHook
    , wsFocusServerModeHook'
    , wsFocusServerModeHook
    ) where

import           Control.Monad (when, forM_, mfilter)
import           Data.Monoid (All(..))
import           XMonad
import qualified XMonad.StackSet as W

-- | Listens for "WS_GO" ClientMessageEvent, extracts the atom name, and runs
-- the given workspace action.
wsGoServerModeHook' :: (WorkspaceId -> X ()) -> Event -> X All
wsGoServerModeHook' handleWs ClientMessageEvent {ev_message_type = mt, ev_data = dt} = do
    wsGo <- getAtom "WS_GO"
    when (mt == wsGo) $ do
      d <- asks display
      forM_ (take 1 dt) $ \a -> do
        name <- io $ getAtomName d (fromIntegral a)
        forM_ (mfilter (not . null) name) handleWs
    return (All True)
wsGoServerModeHook' _ _ = return (All True)

-- | Default 'wsGoServerModeHook' which simply views the target workspace.
wsGoServerModeHook :: Event -> X All
wsGoServerModeHook = wsGoServerModeHook' (windows . W.view)

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
