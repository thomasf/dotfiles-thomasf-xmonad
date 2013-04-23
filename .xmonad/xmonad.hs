module Main (main) where
import XMonad
import XMonad.Config.A00001
import XMonad.Util.Replace (replace)
import Control.Monad (when)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when ("--replace" `elem` args) replace
  xmonad =<< a00001Config
