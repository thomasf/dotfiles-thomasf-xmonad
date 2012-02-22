{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}
{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.A00001.MyKeys
-- Copyright   :  (c) Thomas Frössman 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module specifies my xmonad defaults.
--
------------------------------------------------------------------------
module XMonad.Config.A00001.Util
    ( -- * Usage
      -- $usage
      getScreenDim
    ) where

--import Foreign.C.Types (CInt)
import XMonad
import Graphics.X11.Xinerama
import Control.Monad
import Control.Applicative
import Data.List




--getScreenDim :: Rectangle
getScreenDim = do
    d <- openDisplay ""
    let
      df = defaultScreen d
      si = getScreenInfo d
      ss = liftM head si
      --fe = liftM head ss
      --qsi = getScreenInfo  defaultScreen  d
      --firstScreen = liftM head . liftIO
      firstScreen = ss
    --let
    --    w = displayWidth d s
    --    h = displayHeight d s
    -- TODO:
    --xineramaRects =  getScreenInfo d
    --closeDisplay d
    --return (toInteger w, toInteger h)
    return firstScreen
