module Main (main) where
import XMonad
import XMonad.Config.A00001

main :: IO ()
main = xmonad =<< autoConfig
