-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Host
-- Description :  Host detection and host-specific dispatch utilities
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides utilities for detecting the machine's hostname and matching
-- values or actions based on hostname without hardcoding machine profiles.
-----------------------------------------------------------------------------

module XMonad.Util.Host
    ( getHostname
    , getFullHostname
    , matchHost
    , whenHost
    , whenHosts
    ) where

import           Control.Monad (when)
import           Data.Maybe (fromMaybe)
import           System.Environment (lookupEnv)
import           System.Posix.Unistd (getSystemID, nodeName)
import           XMonad

-- | The current machine's hostname (short form, stripped at first dot):
-- checks $HOST, $HOSTNAME, then POSIX uname.
getHostname :: IO String
getHostname = takeWhile (/= '.') <$> getFullHostname

-- | The full hostname without dot stripping.
getFullHostname :: IO String
getFullHostname = do
  env <- mapM lookupEnv ["HOST", "HOSTNAME"]
  case [v | Just v <- env, not (null v)] of
    h : _ -> return h
    []    -> nodeName <$> getSystemID

-- | Pure lookup by hostname with a fallback default.
-- Useful for startup-time dispatch in layouts or configs.
matchHost :: String -> [(String, a)] -> a -> a
matchHost host matches defVal = fromMaybe defVal (lookup host matches)

-- | Run an X action only if currently running on the given host.
whenHost :: String -> X () -> X ()
whenHost target action = do
  h <- io getHostname
  when (h == target) action

-- | Run an X action only if currently running on one of the given hosts.
whenHosts :: [String] -> X () -> X ()
whenHosts targets action = do
  h <- io getHostname
  when (h `elem` targets) action
