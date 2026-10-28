{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceIPC
-- Description :  ServerMode-style event hook for external workspace/window switching via client messages
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
    ( WsIPCConfig(..)
    , defaultWsIPCConfig
    , wsIPCEventHook
    ) where

import           Control.Monad (forM_, mfilter, when)
import           Data.Monoid (All(..))
import           XMonad
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- | What to do for each message. The workspace handlers get the name in
-- the atom of the first datum, the focus handler the managed window
-- in the first datum.
data WsIPCConfig = WsIPCConfig
  { wsIPCGo     :: WorkspaceId -> X ()
  , wsIPCShift  :: WorkspaceId -> X ()
  , wsIPCRename :: WorkspaceId -> X ()
  , wsIPCFocus  :: Window -> X ()
  }

-- | View, shift to, rename to the workspace and focus the window.
defaultWsIPCConfig :: WsIPCConfig
defaultWsIPCConfig = WsIPCConfig
  { wsIPCGo     = windows . W.view
  , wsIPCShift  = windows . W.shift
  , wsIPCRename = DW.renameWorkspaceByName
  , wsIPCFocus  = windows . W.focusWindow
  }

data WsIPCAtoms = WsIPCAtoms
  { atomGo, atomShift, atomRename, atomFocus :: !Atom }

-- | The atoms, interned on the first message: interning is a round trip
-- to the X server, and client messages are frequent.
newtype WsIPCAtomCache = WsIPCAtomCache (Maybe WsIPCAtoms)

instance ExtensionClass WsIPCAtomCache where
  initialValue = WsIPCAtomCache Nothing

wsIPCAtoms :: X WsIPCAtoms
wsIPCAtoms = do
  WsIPCAtomCache cached <- XS.get
  case cached of
    Just atoms -> return atoms
    Nothing -> do
      atoms <- WsIPCAtoms <$> getAtom "WS_GO" <*> getAtom "WS_SHIFT"
                       <*> getAtom "WS_RENAME" <*> getAtom "WS_FOCUS"
      XS.put (WsIPCAtomCache (Just atoms))
      return atoms

-- | Handles WS_GO, WS_SHIFT, WS_RENAME and WS_FOCUS client messages.
wsIPCEventHook :: WsIPCConfig -> Event -> X All
wsIPCEventHook cfg ClientMessageEvent {ev_message_type = mt, ev_data = a : _} = do
  atoms <- wsIPCAtoms
  let byName handle = do
        d <- asks display
        name <- io $ getAtomName d (fromIntegral a)
        forM_ (mfilter (not . null) name) handle
      w = fromIntegral a
  if | mt == atomGo atoms     -> byName (wsIPCGo cfg)
     | mt == atomShift atoms  -> byName (wsIPCShift cfg)
     | mt == atomRename atoms -> byName (wsIPCRename cfg)
     | mt == atomFocus atoms  -> do
         managed <- gets (W.member w . windowset)
         when managed $ wsIPCFocus cfg w
     | otherwise              -> return ()
  return (All True)
wsIPCEventHook _ _ = return (All True)
