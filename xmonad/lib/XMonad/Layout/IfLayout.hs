{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IfLayout
-- Description :  Choose between two layouts by a condition fixed at startup
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Like "XMonad.Layout.OnHost", but the choice is a Bool decided once when
-- the config is built instead of looking up the host name on every layout
-- run.
-----------------------------------------------------------------------------

module XMonad.Layout.IfLayout
    ( IfLayout
    , ifLayout
    ) where

import           XMonad
import qualified XMonad.StackSet as W

-- | Runs the first layout if the Bool is True, the second otherwise.
data IfLayout l1 l2 a = IfLayout Bool (l1 a) (l2 a)
  deriving (Read, Show)

-- | ifLayout c lt lf is lt if c holds, lf otherwise.
ifLayout :: Bool -> l1 a -> l2 a -> IfLayout l1 l2 a
ifLayout = IfLayout

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (IfLayout l1 l2) a where
  runLayout (W.Workspace i (IfLayout True lt lf) ms) r = do
    (wrs, mlt) <- runLayout (W.Workspace i lt ms) r
    return (wrs, (\lt' -> IfLayout True lt' lf) <$> mlt)
  runLayout (W.Workspace i (IfLayout False lt lf) ms) r = do
    (wrs, mlf) <- runLayout (W.Workspace i lf ms) r
    return (wrs, IfLayout False lt <$> mlf)

  handleMessage (IfLayout True lt lf) m = fmap (\lt' -> IfLayout True lt' lf) <$> handleMessage lt m
  handleMessage (IfLayout False lt lf) m = fmap (IfLayout False lt) <$> handleMessage lf m

  description (IfLayout True lt _) = description lt
  description (IfLayout False _ lf) = description lf
