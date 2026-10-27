{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.EdgeSpacing
-- Description :  Layout modifier adding inner padding while preserving 0 outer margins
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modifier that adds spacing only between windows/panes, with 0 outer margin
-- on edges touching screen borders.
-----------------------------------------------------------------------------

module XMonad.Layout.EdgeSpacing
    ( EdgeSpacing(..)
    , innerSpacing
    ) where

import           XMonad
import           XMonad.Layout.LayoutModifier

-- | Layout modifier that adds spacing only between windows/panes, with 0 outer margin on edges touching screen borders.
data EdgeSpacing a = EdgeSpacing Int Int Int Int Int deriving (Show, Read)

instance LayoutModifier EdgeSpacing a where
  pureModifier (EdgeSpacing 0 0 0 0 0) _ _ wrs = (wrs, Nothing)
  pureModifier (EdgeSpacing oTop oBottom oLeft oRight inner) sRect _mst wrs
    | null wrs || length wrs <= 1 = (wrs, Nothing)
    | otherwise                   = (map adjust wrs, Nothing)
    where
      Rectangle sx sy sw sh = sRect
      sRight  = fromIntegral sx + fromIntegral sw
      sBottom = fromIntegral sy + fromIntegral sh

      adjust (w, Rectangle wx wy ww wh) =
        let wRight  = fromIntegral wx + fromIntegral ww
            wBottom = fromIntegral wy + fromIntegral wh

            t = if wy <= sy + 1           then oTop    else inner
            b = if wBottom >= sBottom - 1 then oBottom else inner
            l = if wx <= sx + 1           then oLeft   else inner
            r = if wRight >= sRight - 1   then oRight  else inner
        in (w, Rectangle (fromIntegral wx + fromIntegral l)
                         (fromIntegral wy + fromIntegral t)
                         (max 1 $ fromIntegral ww - fromIntegral (l + r))
                         (max 1 $ fromIntegral wh - fromIntegral (t + b)))

-- | Apply inner spacing between windows with 0 margins touching screen edges.
innerSpacing :: Int -> l a -> ModifiedLayout EdgeSpacing l a
innerSpacing i = ModifiedLayout (EdgeSpacing 0 0 0 0 i)
