-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Subsequence
-- Description :  Subsequence matching, sorting, and keymap utilities for XPrompt
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides subsequence matching and sorting for XMonad.Prompt, whole-input
-- completion prompt type 'WsPrompt', and an Emacs-like keymap helper.
-----------------------------------------------------------------------------

module XMonad.Prompt.Subsequence
    ( subsequenceQuery
    , subsequenceMatch
    , subsequenceSort
    , WsPrompt(..)
    , emacsLikeXPKeymap'
    ) where

import           Data.Char (toLower, isSpace)
import           Data.List (isSubsequenceOf, tails, sortOn)
import qualified Data.Map as M
import qualified Control.Arrow as Arrow
import           XMonad
import           XMonad.Prompt (Direction1D(..))
import qualified XMonad.Prompt as XP
import qualified XMonad.StackSet as W

-- | The input of a prompt as subsequenceMatch reads it: spaces left out,
-- lower case.
subsequenceQuery :: String -> String
subsequenceQuery = map toLower . filter (not . isSpace)

-- | Prompt completion match: the characters of the input, spaces left
-- out, occur in the completion in that order, ignoring case. "pi2" and
-- "pi 2" match "pi.2".
subsequenceMatch :: String -> String -> Bool
subsequenceMatch input = isSubsequenceOf (subsequenceQuery input) . map toLower

-- | Sorts the completions matched by subsequenceMatch: the shortest
-- stretch holding the input first, then the shortest completion. For
-- "pi2", "pi.2" comes before "pinta.12". An empty input keeps the order.
subsequenceSort :: String -> [String] -> [String]
subsequenceSort input
  | null q    = id
  | otherwise = sortOn (\c -> (tightest (map toLower c), length c))
  where
    q = subsequenceQuery input
    tightest c = minimum (maxBound : [n | t <- tails c, Just n <- [window q t]])
    window [] _ = Just (0 :: Int)
    window _ [] = Nothing
    window qs@(x:xs) (y:ys)
      | qs == q && x /= y = Nothing  -- a stretch starts with a match
      | otherwise         = (+ 1) <$> window (if x == y then xs else qs) ys

-- | A prompt that completes its whole input. Prompts complete only the
-- last space separated word by default, which subsequenceMatch would
-- read as an empty input.
newtype WsPrompt = WsPrompt String

instance XP.XPrompt WsPrompt where
  showXPrompt (WsPrompt s) = s
  commandToComplete _ c = c
  nextCompletion _ = XP.getNextCompletion

-- | Emacs-like XP keymap with a customizable word boundary predicate.
emacsLikeXPKeymap' :: (Char -> Bool) -> M.Map (KeyMask,KeySym) (XP.XP ())
emacsLikeXPKeymap' p = M.fromList $
  map (Arrow.first $ (,) controlMask) -- control + <key>
  [ (xK_z, XP.killBefore) --kill line backwards
  , (xK_k, XP.killAfter) -- kill line fowards
  , (xK_a, XP.startOfLine) --move to the beginning of the line
  , (xK_e, XP.endOfLine) -- move to the end of the line
  , (xK_d, XP.deleteString Next) -- delete a character foward
  , (xK_b, XP.moveCursor Prev) -- move cursor forward
  , (xK_f, XP.moveCursor Next) -- move cursor backward
  , (xK_BackSpace, XP.killWord' p Prev) -- kill the previous word
  , (xK_y, XP.pasteString)
  , (xK_g, XP.quit)
  , (xK_bracketleft, XP.quit)
  ] ++
  map (Arrow.first $ (,) mod1Mask) -- meta key + <key>
  [ (xK_BackSpace, XP.killWord' p Prev)
  , (xK_f, XP.moveWord' p Next) -- move a word forward
  , (xK_b, XP.moveWord' p Prev) -- move a word backward
  , (xK_d, XP.killWord' p Next) -- kill the next word
  , (xK_n, XP.moveHistory W.focusUp')
  , (xK_p, XP.moveHistory W.focusDown')
  ]
  ++
  map (Arrow.first $ (,) 0) -- <key>
  [ (xK_Return, XP.setSuccess True >> XP.setDone True)
  , (xK_KP_Enter, XP.setSuccess True >> XP.setDone True)
  , (xK_BackSpace, XP.deleteString Prev)
  , (xK_Delete, XP.deleteString Next)
  , (xK_Left, XP.moveCursor Prev)
  , (xK_Right, XP.moveCursor Next)
  , (xK_Home, XP.startOfLine)
  , (xK_End, XP.endOfLine)
  , (xK_Down, XP.moveHistory W.focusUp')
  , (xK_Up, XP.moveHistory W.focusDown')
  , (xK_Escape, XP.quit)
  ]
