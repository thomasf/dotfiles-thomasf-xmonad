--------------------------------------------------------------- Headers {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

---------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.SLS
-- Copyright    : (C) Nathaniel Wesley Filardo <nwfilardo@gmail.com>
-- License      : BSD
--
-- Maintainer   : Nathaniel Wesley Filardo <nwfilardo@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides a screen-width-based layout selector: that is, it 
-- is a LayoutModifier which Chooses automatically based on the width of a
-- screen which layout to use.
--
-- I use it to move to a more expanded multi-column layout when a workspace
-- is on a larger monitor and the more compact Tall when not.

module XMonad.Layout.SLS (
    SetPred(..),
    SLS,
    mksls)
where

import           XMonad
import qualified XMonad.Layout.LayoutModifier as LLM
import qualified XMonad.StackSet as S

import Data.List (find)

----------------------------------------------------------------------- }}}
----------------------------------------------------- Utility Functions {{{

-- | Find the screen which is displaying a given workspace tag
findScreenByTag :: WorkspaceId
                -> X (Maybe
                       (S.Screen WorkspaceId (Layout Window)
                                 Window ScreenId ScreenDetail))
findScreenByTag i = gets (S.screens . windowset) 
                  >>= return . find ((== i) . (S.tag . S.workspace))

----------------------------------------------------------------------- }}}
------------------------------------------ Screen-based Layout Selector {{{

  -- | The internal representation of the layout selector.  The fields are
  -- the predicate dimension (see SetPred), the alternate sublayout, and
  -- a flag indicating the last choice taken (since modifyLayoutWithUpdate
  -- can see the screen details but modifyDescription can't; handleMess uses
  -- the last set flag rather than rederiving it.)
data SLS sl a = SLS !Dimension !sl !Bool
  deriving (Show,Read)

  -- | A SetPred message alters the width threshold used to select
  -- layouts.  Perhaps in some future I will get around to making this a
  -- small predicate language rather than a single value.  (It'd be nice to
  -- take a real Haskell predicate function, but those aren't Show-able,
  -- so.)
data SetPred = SetPred Dimension
  deriving (Show,Read,Typeable)
instance Message SetPred

instance (Read (sl a), LayoutClass sl a)
      => LLM.LayoutModifier (SLS (sl a)) a where

    -- glance at the current screen whenever we're asked to modify the
    -- underlying layout; we update internal state to track whether or
    -- not we forward on.  This state is consulted when handling messages
    -- and when printing our description.
    modifyLayoutWithUpdate (SLS sw sl sel)
                           w@(S.Workspace i _ sd)
                             r = do
        mts <- findScreenByTag i
        let sel' = maybe False
                         ((<= sw).rect_width.screenRect.S.screenDetail)
                         mts
        (ows, mol) <- runLayout w r
        (rs,msl) <- if sel'
                     then runLayout (S.Workspace i sl sd) r
                     else return $ (ows, Nothing)
        return ((rs,mol),maybe (if sel' == sel
                                 then Nothing
                                 else Just $ SLS sw sl sel')
                               (\nsl -> Just $ SLS sw nsl sel')
                               msl)

        -- Handle SetPred messages here.
    handleMess (SLS _ sl _) m
     | Just m'@(SetPred ns) <- fromMessage m = do
      io $ putStrLn (show m')
      return . Just $ SLS ns sl False

        -- Otherwise, snag messages if the selector is set and only report
        -- changes if the inner layout reports changes
    handleMess (SLS ms sl sel) m =
      if sel
       then do
            msl <- handleMessage sl m
            return $ maybe Nothing (\nsl -> Just $ SLS ms nsl sel) msl
       else return $ Nothing    -- pass through

    modifyDescription (SLS _ sl True) _ = "SLS " ++ (description sl)
    modifyDescription (SLS _ _ False) l = "SLS " ++ (description  l)

-- | A utility constructor for SLS which helps to get the types right.
mksls :: (LayoutClass sl a)
      => Dimension -> (sl a)
      -> l a -> LLM.ModifiedLayout (SLS (sl a)) l a
mksls sw l = LLM.ModifiedLayout (SLS sw l False)

----------------------------------------------------------------------- }}}
-- vim: tw=76 ts=4 expandtab nu ai foldmethod=marker
