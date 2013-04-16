
module XMonad.Actions.CycleRecentWSAddons (
                                -- * Usage
                                -- $usage
                                cycleWindowSets'
) where

import XMonad hiding (workspaces)
import XMonad.StackSet
import XMonad.Actions.CycleRecentWS


cycref :: [a] -> Int -> a
cycref l i = l !! (i `mod` length l)


-- | Cycle through a finite list of WindowSets with repeated presses of a key, while
--   a modifier key is held down. For best effects use the same modkey+key combination
--   as the one used to invoke this action.
cycleWindowSets' :: (WindowSet -> [WindowSet]) -- ^ A function used to create a list of WindowSets to choose from
                -> [KeySym]                   -- ^ A list of modifier keys used when invoking this action.
                                              --   As soon as one of them is released, the final WindowSet is chosen and the action exits.
                -> KeySym                     -- ^ Key used to preview next WindowSet from the list of generated options
                -> KeySym                     -- ^ Key used to preview previous WindowSet from the list of generated options.
                                              --   If it's the same as nextOption key, it is effectively ignored.
                -> X ()
                                              -- ...
                -> X ()
cycleWindowSets' genOptions mods keyNext keyPrev onswitch = do
  options <- gets $ genOptions . windowset
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
                maskEvent d (keyPressMask .|. keyReleaseMask) p
                KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                s <- keycodeToKeysym d c 0
                return (t, s)
  let setOption n = do windows $ const $ options `cycref` n
                       (t, s) <- io event
                       case () of
                         () | t == keyPress   && s == keyNext  -> onswitch >> setOption (n+1)
                            | t == keyPress   && s == keyPrev  -> onswitch >> setOption (n-1)
                            | t == keyRelease && s `elem` mods -> return ()
                            | otherwise                        -> onswitch >> setOption n
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  setOption 0
  io $ ungrabKeyboard d currentTime
