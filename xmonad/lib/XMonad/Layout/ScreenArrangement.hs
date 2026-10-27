{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ScreenArrangement
-- Description :  Per-host screen splitting and layout arrangement engine
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides reusable tools to define and apply per-host screen arrangements
-- (e.g. splitting physical monitors into virtual panes) while preserving
-- workspace-to-monitor spatial affinity and avoiding redundant re-splits.
-----------------------------------------------------------------------------
module XMonad.Layout.ScreenArrangement
    ( arrangeScreens
    , applyScreenRectangles
    , splitScreenWhere
    , splitSingleScreen
    , splitPane2
    , getHost
    ) where

import           Control.Exception (IOException, try)
import           Control.Monad
import           Data.List (deleteBy, find, sortOn)
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

-- | Transform any rectangle in the list matching a predicate into 2 split panes.
splitScreenWhere :: (Rectangle -> Bool) -> Rational -> [Rectangle] -> [Rectangle]
splitScreenWhere p ratio = concatMap $ \r ->
  if p r then splitPane2 ratio r else [r]

-- | If there is only a single screen, split it into 2 panes; otherwise leave screens as-is.
splitSingleScreen :: Rational -> [Rectangle] -> [Rectangle]
splitSingleScreen ratio [r] = splitPane2 ratio r
splitSingleScreen _ rs      = rs

-- | Run a host-based screen arrangement plan.
arrangeScreens :: (String -> [Rectangle] -> [Rectangle]) -> X ()
arrangeScreens plan = do
  host <- getHost
  physScreens <- withDisplay getCleanedScreenInfo
  applyScreenRectangles (plan host physScreens)

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
