{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports -freduction-depth=99 #-}

module Main (main) where
import XMonad
import XMonad.Config.A00001 (a00001Config)
import XMonad.Util.Replace (replace)
import Control.Monad (when, liftM)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  withArgs args $ xmonad =<< a00001Config
