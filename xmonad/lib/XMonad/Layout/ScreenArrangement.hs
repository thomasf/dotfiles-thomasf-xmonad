{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ScreenArrangement
-- Description :  Per-host screen splitting and layout arrangement
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Manages standard multi-pane screen arrangements per host (e.g. splitting
-- physical monitors into virtual panes with LayoutScreens) while preserving
-- workspace-to-monitor spatial affinity and avoiding unnecessary re-splits.
-----------------------------------------------------------------------------
module XMonad.Layout.ScreenArrangement
    ( standardScreenArrangement
    , applyScreenRectangles
    , splitPane2
    , getHost
    ) where

import           Control.Exception (IOException, try)
import           Control.Monad
import           Data.List (deleteBy, find, sortOn)
import           Data.Maybe (catMaybes)
import           System.Environment (lookupEnv)

import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Util.Run (runProcessWithInput)

-- | Split a rectangle horizontally into 2 panes with a 1px gap, using the given ratio for the left pane.
splitPane2 :: Rational -> Rectangle -> [Rectangle]
splitPane2 ratio (Rectangle vx vy vw vh) =
  let w1  = round (fromIntegral vw * ratio)
      gap = 1
      w2  = max 1 (vw - w1 - fromIntegral gap)
  in [ Rectangle vx vy w1 vh
     , Rectangle (vx + fromIntegral (w1 + gap)) vy w2 vh
     ]

-- | Apply target screen rectangles preserving workspace locations and avoiding redundant reshuffling.
applyScreenRectangles :: [Rectangle] -> X ()
applyScreenRectangles [] = return ()
applyScreenRectangles targetRects = do
  ws <- gets windowset
  let curScreens = W.screens ws
      curRects   = map (screenRect . W.screenDetail) curScreens

  -- If the screen arrangement is already identical, do nothing (no jumping/flickering)
  let rectTuple (Rectangle x y w h) = (x, y, w, h)
  unless (sortOn rectTuple curRects == sortOn rectTuple targetRects) $ do
    windows $ \s ->
      let allVisible = W.current s : W.visible s
          curFocusTag = W.tag (W.workspace (W.current s))
          allWorkspaces = map W.workspace allVisible ++ W.hidden s

          findMatchingWs r (used, avail) =
            let Rectangle rx ry rw rh = r
                cx = fromIntegral rx + fromIntegral rw `div` 2
                cy = fromIntegral ry + fromIntegral rh `div` 2
                matchingScreen = find (\sc -> pointWithin cx cy (screenRect (W.screenDetail sc))) allVisible
                matchingTag = fmap (W.tag . W.workspace) matchingScreen
            in case matchingTag of
                 Just t | t `notElem` used, Just wsp <- find ((== t) . W.tag) avail ->
                   (wsp, (t : used, deleteBy (\a b -> W.tag a == W.tag b) wsp avail))
                 _ ->
                   case avail of
                     (w:ws') -> (w, (W.tag w : used, ws'))
                     []      -> (W.workspace (W.current s), (used, []))

          assign st [] = ([], snd st)
          assign st (tr:trs) =
            let (wsp, st') = findMatchingWs tr st
                (rest, remAvail) = assign st' trs
            in (wsp : rest, remAvail)

          (assignedWs, remainingWs) = assign ([], allWorkspaces) targetRects
          paired = zip targetRects assignedWs

          (focusedPair, otherPairs) = case find ((== curFocusTag) . W.tag . snd) paired of
            Just fp -> (fp, deleteBy (\a b -> fst a == fst b) fp paired)
            Nothing -> case paired of
              (fp:rest) -> (fp, rest)
              []        -> error "assignedWs cannot be empty"

          (r0, w0) = focusedPair
          newCur   = W.Screen w0 0 (SD r0)
          newVis   = zipWith (\(r, w) sid -> W.Screen w sid (SD r)) otherPairs [1..]
      in s { W.current = newCur
           , W.visible = newVis
           , W.hidden  = remainingWs
           }

-- | Standard screen splitting arrangement per host.
-- For "flam": leftmost screen split with 0.33 ratio (M-5 1), center screen split with 0.50 ratio (M-5 2).
-- For "yak": single screen split in two with 0.50 ratio (M-5 2).
standardScreenArrangement :: X ()
standardScreenArrangement = do
  host <- getHost
  physScreens <- withDisplay getCleanedScreenInfo
  let targetRects = case host of
        "flam" | length physScreens >= 4 ->
          let mLeft   = find (\(Rectangle x _ _ _) -> x == 0) physScreens
              mCenter = find (\(Rectangle x y _ _) -> x == 3840 && y >= 1000) physScreens
              mTop    = find (\(Rectangle x y _ _) -> x == 3840 && y < 1000) physScreens
              mRight  = find (\(Rectangle x _ _ _) -> x > 4000) physScreens
              leftPanes   = maybe [] (splitPane2 0.33) mLeft
              centerPanes = maybe [] (splitPane2 0.50) mCenter
              otherPanes  = catMaybes [mTop, mRight]
          in leftPanes ++ centerPanes ++ otherPanes
        "yak" ->
          case physScreens of
            (r:_) -> splitPane2 0.50 r
            []    -> []
        _ ->
          case physScreens of
            [r] -> splitPane2 0.50 r
            _   -> physScreens
  applyScreenRectangles targetRects

-- | Get the current machine's hostname (short form).
getHost :: X String
getHost = io $ do
  mHost <- lookupEnv "HOST"
  case mHost of
    Just h | not (null h) -> return (takeWhile (/= '.') h)
    _ -> do
      res <- try (runProcessWithInput "hostname" [] "") :: IO (Either IOException String)
      case res of
        Right out -> return (takeWhile (\c -> c /= '.' && c /= '\n') out)
        Left _    -> return ""
