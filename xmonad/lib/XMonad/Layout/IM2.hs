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
-- Module      :  XMonad.Layout.IM2
-- Copyright   :  (c) ???
-- License     :  ???
--
-- Maintainer  :  -
-- Stability   :  unstable
-- Portability :  unportable
--
------------------------------------------------------------------------
module XMonad.Layout.IM2
    ( -- * Usage
      -- $usage
      withIMs
      ,hasAnyProperty
      ,applyIMs
    ) where
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
import Control.Monad.Reader
import XMonad.Util.WindowProperties

data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"

-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
  let stack = S.stack wksp
  let ws = S.integrate' $ stack
  rosters <- filterM (hasAnyProperty props) ws
  let n = fromIntegral $ length rosters
  let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
  let rosterRects = splitHorizontally n rostersRect
  let filteredStack = stack >>= S.filter (`notElem` rosters)
  (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
  return (zip rosters rosterRects ++ a, b)
