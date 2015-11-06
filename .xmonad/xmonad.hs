{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports -fcontext-stack=99 #-}

module Main (main) where
import XMonad
import XMonad.Config.A00001
import XMonad.Util.Replace (replace)
import Control.Monad (when, liftM)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  when ("--replace" `elem` args) replace
  newArgs <- resumeArgsFromFile
  withArgs newArgs $ xmonad =<< a00001Config
