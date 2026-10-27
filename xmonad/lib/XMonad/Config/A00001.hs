{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction, ScopedTypeVariables, UndecidableInstances, PostfixOperators #-}
{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports -freduction-depth=99 -Wno-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.A00001
-- Copyright   :  (c) Thomas Frössman 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstableLayoutScreens
-- Portability :  unportable
--
-- This module specifies my xmonad defaults.
--
-- Requires a patched xmonad and xmonad-contrib
--
-----------------------------------------------------------------------------
module XMonad.Config.A00001
    ( -- * Usage
      -- $usage
      a00001Config,
    ) where
import           Control.Exception (IOException, try)
import           Control.Monad
import           Data.Char (isSpace, toLower)
import           Data.IORef
import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           Data.Ratio ((%))
import           Data.Ord
import           Data.Maybe
import           System.Environment (getEnv)
import           System.Exit ( exitSuccess )
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleRecentWSAddons
import           XMonad.Actions.CycleWS hiding (toggleWS, Not)
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseGestures
import qualified XMonad.Actions.Navigation2D as Nav2d
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.PhysicalScreens hiding (rescreen)
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer as WB
import           XMonad.Actions.WithAll
import           XMonad.Hooks.Focus
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(..), docksStartupHook)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook hiding (args)
import           XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import           XMonad.Layout.BoringWindows hiding (Replace, swapUp, swapDown)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.TwoPane
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.LayoutScreens as LS
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OnHost
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Layout.SimpleFloat (simpleFloat)
import qualified XMonad.Layout.Spiral as Spiral
import qualified XMonad.Prompt as XP
import qualified XMonad.StackSet as W hiding (swapUp, swapDown)
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Paste
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import           XMonad.Util.NamedWindows (getName)
import           Text.Printf
import           XMonad.Operations (unGrab)
import qualified Control.Arrow as Arrow
import qualified Graphics.Rendering.Pango.Layout as PL


-- | Layout modifier that adds spacing only between windows/panes, with 0 outer margin on edges touching screen borders.
data EdgeSpacing a = EdgeSpacing Int Int Int Int Int deriving (Show, Read)

instance LayoutModifier EdgeSpacing a where
  pureModifier (EdgeSpacing 0 0 0 0 0) _ _ wrs = (wrs, Nothing)
  pureModifier (EdgeSpacing oTop oBottom oLeft oRight inner) screenRect _mst wrs
    | null wrs || length wrs <= 1 = (wrs, Nothing)
    | otherwise                   = (map adjust wrs, Nothing)
    where
      Rectangle sx sy sw sh = screenRect
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

innerSpacing :: Int -> l a -> ModifiedLayout EdgeSpacing l a
innerSpacing i = ModifiedLayout (EdgeSpacing 0 0 0 0 i)

-- Keyboard configuration:

-- simply for convenience and readability
confModMask = mod4Mask

myKeys xpc conf=
  subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]": mkNamedKeymap conf
  [ ("M-j",             addName "Focus next window on workspace"          _windowFocusDown)
  , ("M-k",             addName "Focus previous window on workspace"      _windowFocusUp)
  , ("M-C-j",           addName "Swap focused with next on workspace"     _windowSwapDownKeepFocus)
  , ("M-C-k",           addName "Swap focused with previous on workspace" _windowSwapUpKeepFocus)
  , ("M-S-j",           addName "Swap focused with next on workspace"     _windowSwapDown)
  , ("M-S-k",           addName "Swap focused with previous on workspace" _windowSwapUp)
  ] ++
  subtitle "Application launching": mkNamedKeymap conf
  [ ("M-o o", spawnh "appmenu")
  , ("M-o <Space>",addName "Goto workspace by window search (ws window)"   $ spawn "ws window")
  , ("M-o c", spawnhm "google-chrome")
  , ("M-o f", spawnhm "firefox")
  , ("M-o u", spawnh' "x.uptime")
  , ("M-o i", spawnh' "x.info")
  , ("M-o j", spawnh' "x.todo")
  , ("M-o w", spawnhm "www")
  , ("M-o d", spawnhm "www-dev")
  , ("M-o t",   spawnh "term")
  , ("M-o S-t", spawnh "env WSENV_SKIP=1 term")
  , ("M-o e", spawnhmw "e")
  , ("M-o n", spawnhm "nautilus")
  , ("M-o a", addName "Run default workspace launcer script" $ bindOnProtectedWorkspace workspaceAction maybeWorkspaceAction)
  ] ++
  subtitle "Other window actions": mkNamedKeymap conf
  [ ("M-<Return>", addName "Swap the focused window and the master window" $ dwmpromote >> movePointer)
  , ("M-t",        addName "Push the window into tiling mode"              $ withFocused (windows . W.sink) >> movePointer)
  , ("M-C-c",      addName "kill"                                          kill')
  , ("M-S-C-c",    addName "run xkill or kill all windows on special workspaces" killPrompt')
  , ("M-u",        addName "Focus urgent winow"                            $ focusUrgent >> movePointer )
  , ("M-C-u",      addName "Clear all urgent window statuses"              $ clearUrgents >> focusUrgent)
  ] ++
  -- subtitle "Screen actions": mkNamedKeymap conf
  -- [ ("M-f",   addName "Next screen"                        $ showWorkspaceNameOld >> nextScreen' >> movePointer >> showWorkspaceNameFast)
  -- , ("M-d",   addName "Previous screen"                    $ showWorkspaceNameOld >> prevScreen' >> movePointer >> showWorkspaceNameFast)
  -- , ("M-C-f", addName "Swap current display witn next"     $ swapNextScreen' >> nextScreen' >> showWorkspaceNameOld >> prevScreen' >> showWorkspaceName >> movePointer )
  -- , ("M-C-d", addName "Swap current display witn previous" $ swapPrevScreen' >> prevScreen' >> showWorkspaceNameOld >> nextScreen' >> showWorkspaceName >> movePointer )
  -- , ("M-S-f", addName "Move window to next screen"         $ shiftNextScreen' >> nextScreen' >> movePointer )
  -- , ("M-S-d", addName "Move window to previous screen"     $ shiftPrevScreen' >> prevScreen' >> movePointer )
  -- ] ++
  subtitle "Horizontal screen actions": mkNamedKeymap conf
  [ ("M-f",   addName "Next screen"                        $ showWorkspaceNameOld >> Nav2d.screenGo R True >> movePointer >> showWorkspaceNameFast)
  , ("M-d",   addName "Previous screen"                    $ showWorkspaceNameOld >> Nav2d.screenGo L True >> movePointer >> showWorkspaceNameFast)
  , ("M-C-f", addName "Swap current display witn next"     $ Nav2d.screenGo R True >> showWorkspaceNameOld >> Nav2d.screenGo L True >> Nav2d.screenSwap R True >> showWorkspaceNameFast)
  , ("M-C-d", addName "Swap current display witn previous" $ Nav2d.screenGo L True >> showWorkspaceNameOld >> Nav2d.screenGo R True >> Nav2d.screenSwap L True >> showWorkspaceNameFast)
  , ("M-S-f", addName "Move window to next screen"         $ Nav2d.windowToScreen R True  >> showWorkspaceNameOld >> Nav2d.screenGo R True >> showWorkspaceNameFast)
  , ("M-S-d", addName "Move window to previous screen"     $ Nav2d.windowToScreen L True  >> showWorkspaceNameOld >> Nav2d.screenGo L True >> showWorkspaceNameFast)
  ] ++
  subtitle "2D Navigation": mkNamedKeymap conf
  [ ("M-<Up>",      addName "Focus window above" $ Nav2d.windowGo U False)
  , ("M-<Down>",    addName "Focus window below" $ Nav2d.windowGo D False)
  , ("M-<Left>",    addName "Focus window left"  $ Nav2d.windowGo L False)
  , ("M-<Right>",   addName "Focus window right" $ Nav2d.windowGo R False)
  -- , ("M-<Alt_L>-<Up>",      addName "Focus screen above" $ Nav2d.screenGo U False)
  -- , ("M-<Down>",    addName "Focus window below" $ Nav2d.windowGo D False)
  -- , ("M-<Left>",    addName "Focus window left"  $ Nav2d.windowGo L False)
  -- , ("M-<Right>",   addName "Focus window right" $ Nav2d.windowGo R False)
  , ("M-C-<Up>",    addName "Swap screen above"  $ Nav2d.screenSwap U False >> showWorkspaceNameOld >> Nav2d.screenGo U False >> showWorkspaceNameFast)
  , ("M-C-<Down>",  addName "Swap screen below"  $ Nav2d.screenSwap D False >> showWorkspaceNameOld >> Nav2d.screenGo D False >> showWorkspaceNameFast)
  , ("M-C-<Left>",  addName "Swap screen left"   $ Nav2d.screenSwap L False >> showWorkspaceNameOld >> Nav2d.screenGo L False >> showWorkspaceNameFast)
  , ("M-C-<Right>", addName "Swap screen right"  $ Nav2d.screenSwap R False >> showWorkspaceNameOld >> Nav2d.screenGo R False >> showWorkspaceNameFast)
  , ("M-S-<Up>",    addName "Swap window above"  $ Nav2d.windowSwap U False)
  , ("M-S-<Down>",  addName "Swap window below"  $ Nav2d.windowSwap D False)
  , ("M-S-<Left>",  addName "Swap window left"   $ Nav2d.windowSwap L False)
  , ("M-S-<Right>", addName "Swap window right"  $ Nav2d.windowSwap R False)
  , ("M-S-C-<Up>",    addName "Drag window to screen above"  $ Nav2d.windowToScreen U False >> showWorkspaceNameOld >> Nav2d.screenGo U False >> showWorkspaceNameFast)
  , ("M-S-C-<Down>",  addName "Drag window to screen below"  $ Nav2d.windowToScreen D False >> showWorkspaceNameOld >> Nav2d.screenGo D False >> showWorkspaceNameFast)
  , ("M-S-C-<Left>",  addName "Drag window to to screen left"   $ Nav2d.windowToScreen L False >> showWorkspaceNameOld >> Nav2d.screenGo L False >> showWorkspaceNameFast)
  , ("M-S-C-<Right>", addName "Drag window to screen right"  $ Nav2d.windowToScreen R False >> showWorkspaceNameOld >> Nav2d.screenGo R False >> showWorkspaceNameFast)
  ] ++
  subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]": mkNamedKeymap conf
  [ ("M-e",         addName "Next workspace (prefix)"     $ rmEmptyWs $ nextWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-r",         addName "Previous workspace (prefix)" $ rmEmptyWs $ prevWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-C-e",       addName "New workspace in prefix.sequence" $ newPrefixWS >> movePointer >> showWorkspaceNameFast)
  , ("M-S-e",       addName "Same workspace in another worktree prompt" $ rmEmptyWs $ worktreePrompt >> movePointer)
  , ("M-S-<Space>", addName "reset layout"                $ setLayout (XMonad.layoutHook conf) >> updateStruts >> movePointer)
  ] ++
  subtitle "Modify current workspace layout... (H/L=size ,.=) [+alt=toggle]": mkNamedKeymap conf
  [ ("M-C-<Space>", addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer >> showLayoutName)
  , ("M-m",         addName "Toggle fullscreen"                                    $ sendMessage (Toggle NBFULL) >> movePointer >> showLayoutName)
  , ("M-n",         addName "Toggle visibiltiy of panels"                          $ sendMessage ToggleStruts >> movePointer)
  , ("M-M1-s",      addName "Toggle visibiltiy of panels"                          $ sendMessage ToggleStruts >> movePointer)
  , ("M-M1-r",      addName "Toggle reflect layout direction"                      $ sendMessage (Toggle REFLECTX) >> movePointer)
  , ("C-M-r",       addName "Toggle reflect layout direction"                      $ sendMessage (Toggle REFLECTX) >> movePointer)
  , ("M-,",         addName "Increment the number of windows in the master area"   $ sendMessage (IncMasterN 1) >> movePointer)
  , ("M-.",         addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)) >> movePointer)
  , ("M-h",         addName "Expand the master area"                               $ sendMessage Expand >> movePointer)
  , ("M-l",         addName "Shrink the master area"                               $ sendMessage Shrink >> movePointer)
  ] ++
  subtitle "Multi media keys": mkNamedKeymap conf
  [ ("<XF86AudioPlay>",         spawnh' "mpc toggle")
  , ("S-<XF86AudioPrev>",       spawnh' "mpc prev")
  , ("S-<XF86AudioNext>",       spawnh' "mpc next")
  , ("<XF86AudioPrev>",         spawnh' "mpc seek -00:00:10")
  , ("<XF86AudioNext>",         spawnh' "mpc seek +00:00:10")
  , ("C-<XF86AudioPrev>",       spawnh' "mpc seek -3%")
  , ("C-<XF86AudioNext>",       spawnh' "mpc seek +3%")
  , ("<XF86AudioLowerVolume>",  spawnh' "pamixer -d 5 ; dunstify -a shorter -u low -r 83723223 \"Volume: $(pamixer --get-volume-human)\"")
  , ("<XF86AudioRaiseVolume>",  spawnh' "pamixer -i 5 ; dunstify -a shorter -u low -r 83723223 \"Volume: $(pamixer --get-volume-human)\"")
  , ("<XF86AudioMute>",         spawnh' "pamixer -t   ; dunstify -a shorter -u low -r 83723223 \"Volume: $(pamixer --get-volume-human)\"")
  , ("<XF86AudioStop>",         spawnh' "mpc stop")
  , ("<XF86MonBrightnessDown>", spawnh' "brightnessctl s 3-% ; dunstify -a shorter -u low -r 83723223 \"$(brightnessctl)\"")
  , ("<XF86MonBrightnessUp>",   spawnh' "brightnessctl s +3% ; dunstify -a shorter -u low -r 83723223 \"$(brightnessctl)\"")
  ] ++
  subtitle "screenshot": mkNamedKeymap conf
  [ ("M-<Print>",     addName "sshot selected to clipboard" $ unGrab >> spawn "sshot")
  , ("M-C-<Print>",   addName "shhot focused window"        $ unGrab >> spawn "sshot focused")
  , ("M-S-<Print>",   addName "shhot full"                  $ unGrab >> spawn "sshot full")
  ] ++
  subtitle "macbook": mkNamedKeymap conf
  [ ("<XF86Eject>",   addName "print "                      $ spawn "xdotool click -clearmodifiers 2")
  , ("M-<XF86Eject>", addName "print screen"                $ sendKey controlMask xK_Print)
  ] ++
  subtitle "named workspace group": mkNamedKeymap conf
  [
    ("M-C-6",         addName "store workspace group 1"     $ addCurrentWSGroup "wsg")
  , ("M-6",           addName "restore workspace group 1"   $ holdScreenFocus $ viewWSGroup "wsg")
  , ("M-C-7",         addName "store workspace group 2"     $ addCurrentWSGroup "wsg2")
  , ("M-7",           addName "restore workspace group 2"   $ holdScreenFocus $ viewWSGroup "wsg2")
  , ("M-C-8",         addName "store workspace group 3"     $ addCurrentWSGroup "wsg3")
  , ("M-8",           addName "restore workspace group 3"   $ holdScreenFocus $ viewWSGroup "wsg3")
  ] ++
  subtitle "screen splitting": mkNamedKeymap conf
  [ ("M-5 2",  addName "Split current screen by 2 panes 0.5"              $ LS.layoutSplitScreen 2 $ innerSpacing 1 (TwoPane 0 0.5))
  , ("M-5 1",  addName "Split current screen by 2 panes 0.7"              $ LS.layoutSplitScreen 2 $ innerSpacing 1 (TwoPane 0 0.33))
  , ("M-5 3",  addName "Split current screen by 3 even columns"           $ LS.layoutSplitScreen 3 $ innerSpacing 1 (Mirror (Tall 3 0 (1/2))))
  , ("M-5 4",  addName "Split current screen by 3 larger center"          $ LS.layoutSplitScreen 3 $ innerSpacing 1 $ (ThreeColMid 1 (3/100) (4/10)))
  , ("M-5 m",  addName "Merge screens 1 + 2"                              $ LS.layoutScreens 2  Full)
  , ("M-5 r",  addName "Reset screens from xinerama, rescreen"              rescreen)
  ] ++
  subtitle "Toggle scratchpads and workspaces": mkNamedKeymap conf
  [ ("M-<Space>",           toggleScratch "largeTerminal")
  , ("M-i a",               myViewWS' "android")
  , ("M-i b",               myViewWS' "bot")
  , ("M-i v",               myViewWS' "vbox")
  , ("M-i c",               myViewWS' "chat")
  , ("M-i d",               myViewWS' "dash")
  , ("M-i f",               myViewWS' "files")
  , ("M-i h",               myViewWS' "home")
  , ("M-i m",               myViewWS' "mail")
  , ("M-i n",               myViewWS' "nodes")
  , ("M-i s",               myViewWS' "s")
  , ("M-i w",               myViewWS' "www")
  , ("M-i j",               myViewWS' "work")
  , ("M-i k",               myViewWS' "work-client")
  , ("M-i q",               myViewWS' "hass")
  , ("M-i M-i",             addName "cycle ws"                              $ rmEmptyWs $ myCycleRecentWs xK_i xK_o)
  , ("M-i i",               addName "Create or change workspace (ws pick)"  $ spawn "ws pick -go")
  -- , ("M-i i",               addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePromptHidden >> maybeWorkspaceAction >> movePointer)
  -- , ("M-C-i M-C-i",         addName "cycle ws on next screen"               $ holdScreenFocus $ nextScreen' >> myCycleRecentWs xK_i xK_o)
  -- , ("M-C-u M-C-u",         addName "cycle ws on prev screen"               $ holdScreenFocus $ prevScreen' >> myCycleRecentWs xK_o xK_i)
  , ("M-i <Space> <Space>", addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
  , ("M-S-i",               addName "Move window to other workspace prompt" $ withWorkspacePrompt xpc (windows . W.shift) >> movePointer >> updateStruts)
  , ("M-i <Space> r",       addName "Rename current workspace"              $ DW.renameWorkspace xpc >> movePointer)
  , ("M-i <Backspace>",     addName "Delete current workspace"              $ DW.removeWorkspace >> movePointer)
  , ("M-p w",               addName ".www"                                  $ gotoPrefixWS "www" >> movePointer)
  , ("M-p d",               addName ".doc"                                  $ gotoPrefixWS "doc" >> movePointer)
  , ("M-p b",               addName ".bot"                                  $ gotoPrefixWS "bot" >> movePointer)
  , ("M-p c",               addName ".code"                                 $ gotoPrefixWS "code" >> movePointer)
  , ("M-p r",               addName ".remote"                               $ gotoPrefixWS "remote" >> movePointer)
  , ("M-p s",               addName ".s"                                    $ gotoPrefixWS "s" >> movePointer)
  , ("M-p f",               addName ".files"                                $ gotoPrefixWS "files" >> movePointer)
  , ("M-p t",               addName ".term"                                 $ gotoPrefixWS "term" >> movePointer)
  , ("M-p l",               addName ".logs"                                 $ gotoPrefixWS "logs" >> movePointer)
  , ("M-p 1",               addName ".1"                                    $ gotoPrefixWS "1" >> movePointer)
  , ("M-p 2",               addName ".2"                                    $ gotoPrefixWS "2" >> movePointer)
  , ("M-p 3",               addName ".3"                                    $ gotoPrefixWS "3" >> movePointer)
  , ("M-p 4",               addName ".4"                                    $ gotoPrefixWS "4" >> movePointer)
  , ("M-p 5",               addName ".5"                                    $ gotoPrefixWS "5" >> movePointer)
  , ("M-p 6",               addName ".6"                                    $ gotoPrefixWS "6" >> movePointer)
  , ("M-p 7",               addName ".7"                                    $ gotoPrefixWS "7" >> movePointer)
  , ("M-p 8",               addName ".8"                                    $ gotoPrefixWS "8" >> movePointer)
  , ("M-p 9",               addName ".9"                                    $ gotoPrefixWS "9" >> movePointer)
  , ("M-p 0",               addName ".0"                                    $ gotoPrefixWS "0" >> movePointer)
  , ("M-p p",               addName "base" gotoBaseWS)
 ] ++
 subtitle "dunst controls": mkNamedKeymap conf
 [ ("M-1",             addName "close dunst popup"                       $ spawn "dunstctl close")
 , ("M-9",             addName "context action"                          $ spawn "dunstctl action")
 , ("M-0",             addName "reopen dunst popup"                      $ spawn "dunstctl history-pop")
 ] ++
 subtitle "exit/quit/leave/reboot...": mkNamedKeymap conf
 [ ("M-q r",             addName "restart xmonad"                       $ restart "xmonad" True)
 , ("M-q x x x",         addName "restart xmonad without keeping state" $ restart "xmonad" False)
 , ("M-q k k k",         addName "KILL xmonad"                          $ io exitSuccess)
 , ("M-q <Space>",       addName "xmenu"                                $ spawn "xmenu")
 , ("M-q d",             addName "display off"                          $ spawn "a.display-off")
 , ("M-q q",             addName "display off"                          $ spawn "a.display-off -nowait")
 , ("M-q l",             addName "leave"                                $ spawn "a.leave")
 , ("M-q p",             addName "restart panels"                       $ spawn "panels")
 ]
  where
    spawnh' cmd' = addName cmd'           $ spawn cmd'
    spawnh  cmd' = addName cmd' $ bindOn' $ spawn cmd'
    spawnhm cmd' = addName cmd' $ bindOn' $ showAppName cmd' >> spawn cmd'
    -- | Like spawnhm, with WS_LAUNCH set so that emacs starts in the
    -- workspace directory with its environment.
    spawnhmw cmd' = addName cmd' $ bindOn' $ showAppName cmd' >> spawn ("env WS_LAUNCH=1 " ++ cmd')

    -- | Remove current workpace if empty
    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept keptWorkspaces


    -- | View a workspace by name, remove left over empty workspace and move pointer
    myViewWS' wsid = addName("Show " ++ wsid ++ " workspace ") $ do
      rmEmptyWs $ myViewWS wsid
      movePointer
    wsPrompt wsp = wsp xpc afterWSPrompt

    -- | Select workspae prompt
    selectWorkspacePrompt = wsPrompt workspacePromptAll

    -- | Prompt for any workspace, sorted by index
    workspacePromptAll :: XP.XPConfig -> (String -> X ()) -> X ()
    workspacePromptAll c job = do ws <- gets (W.workspaces . windowset)
                                  s <- getSortByIndex
                                  XP.mkXPrompt (WsPrompt "") c (XP.mkComplFunFromList' c (map W.tag $ s ws)) job

    -- | Prompt for a workspace, created hidden if it is new, and run job on it
    withWorkspacePrompt :: XP.XPConfig -> (String -> X ()) -> X ()
    withWorkspacePrompt c job = workspacePromptAll c $ \t -> do
      exists <- gets (W.tagMember t . windowset)
      unless exists $ DW.addHiddenWorkspace t
      job t

    -- The XPrompt M-i i, replaced by ws pick:
    -- -- | Select workspace prompt
    -- selectWorkspacePromptHidden = wsPrompt workspacePromptHidden
    -- -- selectWorkspacePromptHidden = workspacePrompt'
    --
    -- -- | Select workspace promt only showing non visible workspaces
    -- workspacePromptHidden :: XP.XPConfig -> (String -> X ()) -> X ()
    -- workspacePromptHidden c job = do ws <- gets (W.hidden . windowset)
    --                                  s <- getSortByIndex
    --                                  -- thisWS <- gets (W.currentTag . windowset)
    --                                  let ts = map W.tag $ s ws
    --                                      -- prefix = takeWhile (/= wsSeparator) thisWS
    --                                      prefix = ""
    --                                  XP.mkXPrompt (WsPrompt "") c {XP.defaultText=prefix} (XP.mkComplFunFromList' c ts ) job


    -- | Toggle scratch pad
    toggleScratch cmd' = addName("Toggle " ++ cmd' ++ " scratchpad ") $ namedScratchpadAction myScratchPads cmd'

    wsSeparator = '.'

    gotoBaseWS :: X ()
    gotoBaseWS = withWindowSet $ \w -> do
      thisWS <- gets (W.currentTag . windowset)
      let wss = W.workspaces w
          currentTagPrefix = takeWhile (/= wsSeparator) thisWS
          new = currentTagPrefix
      unless (new `elem` map W.tag wss) $ myViewWS new
      windows $ W.view new

    gotoPrefixWS :: String -> X ()
    gotoPrefixWS suffix = withWindowSet $ \w -> do
      thisWS <- gets (W.currentTag . windowset)
      let wss = W.workspaces w
          currentTagPrefix = takeWhile (/= wsSeparator) thisWS
          new = currentTagPrefix ++ "." ++ suffix
      unless (new `elem` map W.tag wss) $ myViewWS new
      windows $ W.view new

    -- | Prompt for a worktree of the project and go to the same workspace
    -- (same suffix) in it: hello.bot -> hello@test.bot. Completes on
    -- "ws wt list" and existing workspaces of the project. A new name goes
    -- to project@name, where ws action offers to create the worktree.
    worktreePrompt :: X ()
    worktreePrompt = do
      w <- gets windowset
      let thisWS = W.currentTag w
          wss = W.workspaces w
          (prefix, suffix) = break (== wsSeparator) thisWS
          projectOf = takeWhile (/= '@') . takeWhile (/= wsSeparator)
          project = projectOf thisWS
          existing = [takeWhile (/= wsSeparator) (W.tag ws) | ws <- wss, projectOf (W.tag ws) == project]
      listed <- lines <$> runProcessWithInput "sh" ["-c", "exec ws wt list \"$1\" 2>/dev/null", "sh", thisWS] ""
      let prefixes = delete prefix $ nub (listed ++ sort existing)
          go input = do
            let p = takeWhile (/= wsSeparator) input
                target = (if '@' `elem` p || p == project then p else project ++ "@" ++ p) ++ suffix
            unless (null p || target == thisWS) $ do
              unless (target `elem` map W.tag wss) $ myViewWS target
              windows $ W.view target
      XP.mkXPrompt (WsPrompt ("worktree" ++ suffix ++ ": ")) xpc (XP.mkComplFunFromList' xpc prefixes) go

    wsi = WSIs wsPred
     where
       wsPredHidden = do hs <- gets (map W.tag . W.hidden . windowset)
                         return (\w -> W.tag w `elem` hs)

       wsPredGroup = do cur <- (groupName . W.workspace . W.current) `fmap` gets windowset
                        return $ (cur ==).groupName
                        where groupName = takeWhile (/= wsSeparator) . W.tag

       wsPred = do tg <- wsPredGroup
                   hi <- wsPredHidden
                   return (\w -> tg w && hi w)

    -- |  Select next workspace with same prefix
    nextWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Next wsi 1

    -- | Select previous workspac with same prefix
    prevWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Prev wsi 1

    -- | Create a new prefixed sub workspace
    newPrefixWS :: X ()
    newPrefixWS = withWindowSet $ \w -> do
      thisWS <- gets (W.currentTag . windowset)
      let wss = W.workspaces w
          currentTagPrefix = takeWhile (/='.') thisWS

          cws = map W.tag $ filter (\ws -> (currentTagPrefix ++ ".") `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
          num = head $ [0..] \\ mapMaybe (readMaybe . drop (length currentTagPrefix +1 )) cws :: Integer
          new = printf "%s%c%i" currentTagPrefix wsSeparator num
      unless (new `elem` map W.tag wss) $ myViewWS new
      windows $ W.view new
        where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing

   -- | Create a new prefixed sub workspace
    -- newPrefixWSName wsid
    newPrefixWSName wsid = withWindowSet $ \w -> do
    -- newPrefixWSName wsid = do
      let wss = W.workspaces w
          currentTagPrefix = takeWhile (/='.') wsid
          cws = map W.tag $ filter (\ws -> (currentTagPrefix ++ ".") `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
          num = head $ [0..] \\ mapMaybe (readMaybe . drop (length currentTagPrefix +1 )) cws :: Integer
          new = printf "%s%c%i" currentTagPrefix wsSeparator num
      return new
        where readMaybe s = case reads s of
                [(r,_)] -> Just r
                _       -> Nothing


    -- | filter some workspaces
    filterSomeWorkspaces = fmap (.namedScratchpadFilterOutWorkspace
                                 .myFilterOutWorkspace "chat"
                                 .myFilterOutWorkspace "dash")
    myFilterOutWorkspace :: String -> [WindowSpace] -> [WindowSpace]
    myFilterOutWorkspace wsname = filter (\(W.Workspace tag _ _) -> tag /= wsname)

    cycleRecentWS' = cycleWindowSets' options
     where options w = map (`W.view` w) (recentTags w)
           recentTags w = filterSomeWorkspaces map W.tag $ W.hidden w ++ [W.workspace (W.current w)]

    -- | Cycle recent ws
    myCycleRecentWs keyForward keyBackward = cycleRecentWS'
                      [ xK_Alt_L, xK_Alt_R
                      , xK_Super_L, xK_Super_R
                      , xK_Hyper_L, xK_Hyper_R
                      , xK_Control_L, xK_Control_R]
                      keyForward keyBackward showWorkspaceNameFast

    -- | Sort workspaces by tag name, exclude hidden scrachpad workspace.
    getSortByTagNoSP = filterSomeWorkspaces getSortByTag

    -- The window search M-o <Space>, replaced by ws window:
    -- -- | Returns the window name as will be listed in dmenu.
    -- --   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
    -- --   know where he's going.
    -- decorateName ws w = do
    --   wName <- show <$> getName w
    --   cName <- getClass w
    --   let wm = PL.escapeMarkup wName
    --       cn = PL.escapeMarkup cName
    --       tn = PL.escapeMarkup (W.tag ws)
    --   return $ printf "%s<span font_family='Consolas'> — %s</span> ❨%s❩" wm cn tn
    --
    -- getClass w = do
    --   classHint <- withDisplay $ \d -> io $ getClassHint d w
    --   return $ resClass classHint
    --
    -- gotoWindow = WB.gotoMenuConfig def {
    --   WB.menuCommand="rofi",
    --   WB.menuArgs=["-markup-rows", "-dmenu", "-i", "-no-custom"],
    --   windowTitler=decorateName }




-- | Mouse bindings
myMouseBindings =
    [ ((0, button10), mouseGesture gestures)
    , ((confModMask, button4), \_ -> windows W.focusUp)
    , ((confModMask, button5), \_ -> windows W.focusDown)
      -- -- figure out what shold be bound here
      -- , ((0, button8), \_ -> _windowRotategAllDown)
      -- , ((0, button9), \_ -> _windowRotateAllUp)
    ]
    where
      -- button8 =  8 :: Button
      -- button9 =  9 :: Button
      button10 =  10 :: Button
      gestures = M.fromList
                 [ ([    ], \_ -> io readPalette >>= \pal -> gridselectWorkspace (myGsconfig pal) W.greedyView)
                 , ([R, D], \_ -> sendMessage NextLayout)
                 , ([U   ], \w -> focus w >> Nav2d.windowSwap U False)
                 , ([D   ], \w -> focus w >> Nav2d.windowSwap D False)
                 , ([L   ], \w -> focus w >> Nav2d.windowSwap L False)
                 , ([R   ], \w -> focus w >> Nav2d.windowSwap R False)
                 ]




-- | Colors

-- | The colors of the theme in use, by name, such as bg_0, fg_1 or magenta,
-- with the mode as "mode". dotfiles writes them to
-- ~/.config/theme/xmonad-palette, as a Haskell list of pairs, when it
-- installs or switches the theme; switching restarts xmonad.
type Palette = M.Map String String

{-# NOINLINE paletteRef #-}
paletteRef :: IORef (Maybe Palette)
paletteRef = unsafePerformIO (newIORef Nothing)

readPalette :: IO Palette
readPalette = do
  cached <- readIORef paletteRef
  case cached of
    Just pal -> return pal
    Nothing  -> do
      pal <- readPaletteFromFile
      writeIORef paletteRef (Just pal)
      return pal

readPaletteFromFile :: IO Palette
readPaletteFromFile = do
  home <- getEnv "HOME"
  s <- try (readFile $ home ++ "/.config/theme/xmonad-palette") :: IO (Either IOException String)
  return $ case s of
    Right str | [(ps, rest)] <- reads str, all isSpace rest -> M.fromList ps
    _ -> M.empty

-- | col p name is the color name of p, or grey if p does not have it, as
-- before dotfiles has written the palette.
col :: Palette -> String -> String
col p name = M.findWithDefault "#808080" name p

myFocusedColor pal = col pal "magenta"
myUrgentColor pal = col pal "blue"
myNormalBorderColor pal = col pal "bg_1"
-- | GridSelect in the colors of the palette, like rofi and the prompts: the
-- cells are a raised surface, bg_1 with fg_1 text, the selected one magenta
-- with bg_0 text, and bg_0 separates them instead of white outlines.
myGsconfig :: Palette -> GSConfig a
myGsconfig pal = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 70
    , gs_cellwidth = 150
    , gs_cellpadding = 6
    , gs_font = sizedXftFont "13"
    , gs_bordercolor = col pal "bg_0"
    }
  where
    colorizer _ active = return $ if active
      then (col pal "magenta", col pal "bg_0")
      else (col pal "bg_1", col pal "fg_1")

myDefaultSpacerWidth = 3
myDefaultBorderWidth = 4

-- | Fonts
sizedXftFont px = "xft:PragmataPro:pixelsize=" ++ px
sizedFont px = "-xos4-terminus-*-r-*-*-" ++ px  ++ "-*-*-*-*-*-iso8859-*"
largeFont = sizedFont "32"



-- | Workspaces
myWorkspaces = [ "s", "s.0", "s.1", "s.2", "s.3", "s.4"]
myTerminal = "term"

-- | Layout hook
myLayoutHook =
  onWorkspace "video" (renameStar full) .
  onWorkspace "vbox" (renameStar full) .
  onWorkspace "android" (renameStar simpleFloat) .
  avoidStruts .
  mkToggle (single NBFULL) .
  boringWindows .
  onWorkspace "chat" (chat ||| gridWide) .
  onWorkspace "music" tabs .
  onWorkspace "files" (grid ||| tabs) .
  onWorkspace "nodes" (renameStar tabs) .
  onWorkspace "dash" (dash ||| grid) .
  onWorkspace "work" (fullBorder ||| tabs ||| gridWide) .
  onWorkspace "mail" (fullBorder ||| gridWide) .
  onWorkspace "cal" (fullBorder ||| gridWide) .
  onWorkspaces ["work-client", "work-client.1", "work-client.2", "work-client.3"] (fullBorder ||| tabs ||| gridWide) .
  onWorkspaces ["tl.jira"] (fullBorder ||| tabs ||| gridWide) .
  onWorkspace "upgrade" alternative $
  lessBorders OnlyScreenFloat
  standard
  where
    -- control
    first = wide
    standard = onTall
               (first ||| tall ||| tcol ||| grid ||| gridWide )
               (first ||| tall ||| tabs ||| gridWide ||| spiral ||| oneBig)
    alternative = onTall
               (tcol ||| first ||| grid)
               (first ||| tall ||| tabs ||| gridWide ||| spiral ||| oneBig)
    onTall = onHosts ["transwhale", "flam", "yak"]
    -- helpers
    refmin = mkToggle (single REFLECTX)
    mySpacing = innerSpacing
    ss = mySpacing myDefaultSpacerWidth
    rename name' = renamed [Replace name']
    renameStar = renamed [Replace "*"]

    -- layouts
    oneBig = rename "1big" . ss . refmin $ OneBig (3/4) (3/4)
    chat = rename "chat" . ss . refmin $ fullscreenFull Full
    fullBorder = rename "full" . ss . refmin $ fullscreenFull Full
    full = rename "full" $ noBorders (fullscreenFull Full)
    wide' nm ir = rename "wide" . ss . Mirror . refmin $ Tall nm (3/100) ir
    wide = onTall (wide' 1 (7/8)) (wide' 2 (4/5))
    tall = rename "tall" . ss . refmin $ Tall 2 (3/100) (3/5)
    tcol = rename "3col" . ss . Mirror $ ThreeColMid 1 (3/100) (4/6)
    dash = rename "dash" $ onTall tcol wide
    spiral = rename "spiral" . ss . refmin $ Spiral.spiral (6/7)
    tabs = rename "tabs" . ss . Mirror $ Tall 1 0 0.93
    gridWide = rename "gridw" . ss . refmin $ GridRatio (16/9)
    grid = rename "grid" . ss . refmin $ GridRatio (4/3)


-- Window rules:
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook :: ManageHook

myManageHook =
  fullscreenManageHook <+> namedScratchpadManageHook myScratchPads <+> (composeOne . concat $
  [ [resource  =? r -?>                                        doIgnore           | r <- ["desktop_window", "kdesktop", "Panel"]]
  , [className =? c -?>                                        doIgnore           | c <- ["Ediff", "Unity-2d-panel", "Xfce4-notifyd", "Xfdesktop"]]
  -- , [className =? c -?>                                        doSink             | c <- ["emulator64-mips", "emulator-arm", "emulator-x86"
  --                                                                                        ,"emulator64-arm", "emulator64-x86", "emulator-mips"]]
  , [wmName    =? "Emulator" -?> doFloat]
  , [isTooltip -?>                                             doIgnore]
  , [className =? c <&&> skipTaskbar  -?>                      doIgnore           | c <- ["UE4Editor", "com-eteks-sweethome3d-SweetHome3D"]]
  , [isDialog  <&&> className =? "org.gnome.Nautilus" -?>      myCenterFloat 0.5 0.3]
  , [resource  =? r -?>                                        doFloat            | r <- ["floating"]]
  , [className =? c -?>                                        doFloat            | c <- ["Unity-2d-launcher", "Orage", "feh"]]
  -- NOTE chrome does note set title early enough for this
  -- , [role      =? "pop-up" <&&> appName =? "google-chrome" <&&> startsWith' title "Developer Tools - "  -?> doSink]
  , [role      =? "pop-up" <&&> appName =? "google-chrome" -?> doCenterFloatLarge]
  , [role      =? "GtkFileChooserDialog" -?> doCenterFloatLarge]
  , [role      =? "bubble" -?>                                 doIgnore]
  , [className =? "Zenity" <&&> title =? "Question" -?>        doCenterFloat]
  , [className =? "Zenity" -?>                                 doCenterFloatLarge]
  , [className =? "zenity" <&&> title =? "Question" -?>        doCenterFloat]
  , [className =? "zenity" -?>                                 doCenterFloatLarge]
  , [className =? c -?>                                        doCenterFloat      | c <- ["Xfce4-settings-manager", "Pinentry", "connected-app"]]
  , [className =? c -?>                                        doCenterFloatLarge | c <-  ["Xfce4-appfinder"]]
  , [resource  =? c -?>                                        doCenterFloatLarge | c <-  ["floating-center-large"]]
  , [resource  =? c -?>                                        doCenterFloat      | c <-  ["floating-center"]]
  , [className =? "Onboard"  -?>                               doIgnore]
  , [title     =? "screenkey"  -?>                             doIgnore]
  , [transience]
  , [resource  =? "xmessage" -?>                               doCenterFloatLarge]
  ])
  where

    role = stringProperty "WM_WINDOW_ROLE"
    wmName = stringProperty "WM_WINDOW_NAME"
    doCenterFloatLarge = myCenterFloat 0.95 0.8

    -- doSink = ask >>= doF . W.sink
    skipTaskbar = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR"
    -- isAbove = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE"
    isTooltip = stringProperty "_NET_WM_WINDOW_TYPE" =? "_NET_WM_WINDOW_TYPE_TOOLTIP"
    startsWith' :: Eq a => Query [a] -> [a] -> Query Bool
    startsWith' q prefix = fmap (isPrefixOf prefix) q



-- Event handling

-- Defines a <custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook = noRescreenEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook <+> serverModeEventHook <+> wsGoEventHook <+> wsFocusEventHook


noRescreenEventHook ConfigureEvent {ev_window = w} = do
    r <- asks theRoot
    return $ All $ w /= r

noRescreenEventHook _ = mempty

-- | Workspaces kept when they are left empty
keptWorkspaces :: [WorkspaceId]
keptWorkspaces = [ "NSP", "", "s.0", "s.1", "s.2", "s.3", "s.4" ]

-- | Go to the workspace "ws go" names, as the workspace prompt did: a
-- WS_GO client message whose first datum is an atom with the workspace
-- name. A new workspace is created, an empty one gets its workspace
-- action, and the workspace left is removed if empty. EWMH can only
-- switch to existing workspaces.
wsGoEventHook :: Event -> X All
wsGoEventHook ClientMessageEvent {ev_message_type = mt, ev_data = dt} = do
    d <- asks display
    wsGo <- io $ internAtom d "WS_GO" False
    when (mt == wsGo) $ forM_ (take 1 dt) $ \a -> do
      name <- io $ getAtomName d (fromIntegral a)
      forM_ (mfilter (not . null) name) $ \n ->
        DW.removeEmptyWorkspaceAfterExcept keptWorkspaces $ do
          exists <- gets (W.tagMember n . windowset)
          if exists
            then showWorkspaceNameOld >> windows (W.view n) >> maybeWorkspaceAction >> showWorkspaceNameFast
            else myViewWS n
          movePointer
    return (All True)
wsGoEventHook _ = return (All True)

-- | Focus the window "ws window" names, as the window search did: a
-- WS_FOCUS client message whose first datum is the window. Its
-- workspace is shown if need be.
wsFocusEventHook :: Event -> X All
wsFocusEventHook ClientMessageEvent {ev_message_type = mt, ev_data = dt} = do
    d <- asks display
    wsFocus <- io $ internAtom d "WS_FOCUS" False
    when (mt == wsFocus) $ forM_ (take 1 dt) $ \a -> do
      let w = fromIntegral a
      managed <- gets (W.member w . windowset)
      when managed $ windows (W.focusWindow w) >> movePointer
    return (All True)
wsFocusEventHook _ = return (All True)


-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myXmobarTopPP pal = def
  { ppCurrent = xmobarColor (myFocusedColor pal) "" . wrap " " "" . trim
  , ppVisible = wrap " " "" . trim
  , ppHidden  = const ""
  , ppUrgent  = wrap " ◀"  "▶"
  , ppTitle   = const ""
  , ppLayout  = const ""
  , ppSep     = " "
  , ppSort    = mySortByXineramaPhysicalRule
  }

myXmobarBottomPP pal = def
  { ppCurrent = const ""
  , ppVisible = const ""
  , ppHidden  = const ""
  , ppUrgent  = const ""
  , ppTitle   = trim
  , ppLayout  = const ""
  , ppSep     = xmobarColor (col pal "cyan") "" " + "
  , ppSort    = mySortByXineramaPhysicalRule
  }

myLogHook pal = do
  dynamicLogString (myXmobarTopPP pal) >>= xmonadPropLog' "_XMONAD_LOG_TOP"
  dynamicLogString (myXmobarBottomPP pal) >>= xmonadPropLog' "_XMONAD_LOG_BOTTOM"
  workspaceHistoryHook
  ewmhDesktopsLogHook
  -- desktopLogHook
  -- fadeInactiveLogHook 0.86


-- | 'logHook' preserving old 'ewmh' behavior to switch workspace and focus to
-- activated window.
-- desktopLogHook :: X ()
-- -- desktopLogHook      = activateLogHook (reader W.focusWindow >>= doF)


-- | Like 'getSortByXineramaRule', but uses physical locations for screens.
mySortByXineramaPhysicalRule :: X WorkspaceSort
mySortByXineramaPhysicalRule = mkWsSort myXineramaPhysicalWsCompare
-- | A comparison function like 'getXineramaWsCompare', but uses physical locations for screens.

myXineramaPhysicalWsCompare :: X WorkspaceCompare
myXineramaPhysicalWsCompare = myXineramaWsCompare' True

-- | Modified to sort (x,y) instead of (y,x)
myXineramaWsCompare' :: Bool -> X WorkspaceCompare
myXineramaWsCompare' phy = do
    w <- gets windowset
    return $ \ a b -> case (isOnScreen a w, isOnScreen b w) of
        -- (True, True)   -> cmpPosition phy w a b
        (True, True)   -> cmpPosition phy w a b
        (False, False) -> compare a b
        (True, False)  -> LT
        (False, True)  -> GT
  where
    onScreen w = W.current w : W.visible w
    isOnScreen a w = a `elem` map (W.tag . W.workspace) (onScreen w)
    findScreen s x = find ((== x) . W.tag . W.workspace) s
    cmpPosition False w = comparing (fmap W.screen . findScreen (onScreen w))
    cmpPosition True  w = comparing (fmap (screenPos . W.screenDetail) . findScreen (onScreen w))
      where screenPos sd = let Rectangle x y _ _ = screenRect sd in (x, y)


physicalScreenOrder= horizontalScreenOrderer

nextScreen' = onNextNeighbour  physicalScreenOrder W.view
prevScreen' = onPrevNeighbour physicalScreenOrder W.view
swapNextScreen' = onNextNeighbour physicalScreenOrder W.greedyView
swapPrevScreen' = onPrevNeighbour physicalScreenOrder W.greedyView
shiftNextScreen' = onNextNeighbour physicalScreenOrder W.shift
shiftPrevScreen' = onPrevNeighbour physicalScreenOrder W.shift


-- Startup hook

myStartupHook = do
  ewmhDesktopsStartup
  setWMName "LG3D"
  return ()



-- Urgency hook

myUrgencyHook pal =
  withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = myUrgentColor pal }
    urgencyConfig
      { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }



-- XMonad Prompt configuration

defXPConfig = def
 { XP.position = XP.CenteredAt 0.4 0.5
 , XP.promptBorderWidth = 1
 , XP.font = sizedXftFont "20"
 , XP.height = 24
 , XP.promptKeymap = emacsLikeXPKeymap' (\c -> isSpace c || c == '.')
 , XP.searchPredicate = subsequenceMatch
 , XP.sorter = subsequenceSort
 }

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
-- | Prompts are a raised surface, like rofi: bg_1 with fg_1 text.
myXPConfig pal = defXPConfig
 { XP.bgColor = col pal "bg_1"
 , XP.fgColor = col pal "fg_1"
 , XP.bgHLight = col pal "bg_1"
 , XP.fgHLight = col pal "magenta"
 , XP.borderColor = col pal "bg_1"
 }

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



-- Scratch pads:

myScratchPads = [ NS "largeTerminal" (term "largeTerminal") (res =? scratch "largeTerminal") $ myCenterFloat 0.95 0.8
                , termScratch "pamixer" $ myCenterFloat 0.7 0.2
                , termScratch "htop" $ myCenterFloat 0.95 0.9
                ]
  where
    scratch sname = "scratchpad_" ++ sname
    -- term sname = " urxvt " ++ " -name " ++ scratch sname
    -- WSENV_SKIP keeps the shell from sourcing the environment of the
    -- workspace the scratchpad happens to be opened on.
    term sname = " env WSENV_SKIP=1 alacritty " ++ " --class " ++ scratch sname
    termScratch scmd = NS scmd (inTerm' scmd scmd) (res =? scratch scmd)
    inTerm' sname scmd = term sname ++ " -e " ++  scmd
    res = resource


myCenterFloat w h = customFloating $ W.RationalRect left top width height
  where
    width = w
    height = h
    left = (1 - w) / 2
    top = (1 - h) / 2

myNavigation2DConfig = def {
  Nav2d.defaultTiledNavigation =  Nav2d.centerNavigation }


--  a00001Config


a00001Config = do
  pal <- io readPalette
  return . docks . Nav2d.withNavigation2DConfig myNavigation2DConfig . myUrgencyHook pal . addDescrKeys' ((confModMask, xK_F1), showKeybindings) (myKeys $ myXPConfig pal) $ def {
    terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = myDefaultBorderWidth
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor pal
  , focusedBorderColor = myFocusedColor pal
  , keys               = emptyKeys
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , startupHook        = myStartupHook
  , logHook = myLogHook pal
  } `additionalMouseBindings` myMouseBindings
   where
    --  | An empty keymap
    emptyKeys c = mkKeymap c [ ]

    -- | Display keyboard mappings using zenity
    showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
    showKeybindings x = addName "Show Keybindings" $ io $ do
      h <- spawnPipe "zenity --text-info --font='Ubuntu Mono'"
      hPutStr h (unlines $ showKm x)
      hClose h
      return ()


-- | Working versions of swapup/swapdown
swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (rot $ reverse rs) []
    where rot (x:xs) = xs ++ [x]
          rot _ = []

swapUp = W.modify' swapUp'

reverseStack (W.Stack t ls rs) = W.Stack t rs ls

swapDown = W.modify' (reverseStack . swapUp' . reverseStack)

-- | main window management functions
_windowFocusDown = windows W.focusDown >> movePointer
_windowFocusUp = windows W.focusUp >> movePointer
_windowSwapDownKeepFocus = windows swapDown >> windows W.focusUp >> movePointer
_windowSwapUpKeepFocus = windows swapUp >> windows W.focusDown >> movePointer
_windowSwapDown = windows swapDown >> movePointer
_windowSwapUp = windows swapUp >> movePointer
_windowRotateAllDown = rotAllDown >> movePointer
_windowRotateAllUp = rotAllUp >> movePointer

-- | Move mouse pointer to bottom right of the current window
movePointer = updatePointer (0.99, 0.99) (0, 0)



-- | bind keys but not for some protected workspaces
bindOnProtectedWorkspace cmd' altCmd  = bindOn
    [ ("work", altCmd)
    , ("work-client", altCmd)
    , ("dash", altCmd)
    , ("chat", altCmd)
    , ("mail", altCmd)
    , ("home", altCmd)
    , ("NSP", altCmd)
    , ("nodes", altCmd)
    , ("", cmd')]


bindOn' x  = bindOnProtectedWorkspace x showWorkspaceNameFast


holdScreenFocus a = do
   s <- gets (W.screen . W.current . windowset)
   r <- a
   screenWorkspace s >>= maybe (return ()) (windows . W.view)
   return r


-- | kill window with some exceptions
kill' :: X ()
kill' = withFocused $ \win -> do
  appn <- runQuery appName win
  titl <- runQuery title win
  res <- runQuery resource win
  role <- runQuery (stringProperty "WM_WINDOW_ROLE") win
  killOrElse win appn titl res role
    where
  alwayskillable titl = "Developer Tools -" `isPrefixOf` titl
  -- unkillable appn titl res = appn `elem` ["ssh_tmux"] || titl == "XMonad" || res == "floating-center-large"
  unkillable appn titl res = appn == "emacs" || appn == "ssh_tmux" || titl == "XMonad" || res == "floating-center-large"
  scratchTerm appn = appn == "scratchpad_largeTerminal"
  askToKill appn role = appn == "google-chrome" && role == "browser" || appn == "urxvt" || appn == "Alacritty" || appn == "alacritty"
  killOrElse win appn titl res role
    | alwayskillable titl = killWindow win
    | unkillable appn titl res = return ()
    | scratchTerm appn = namedScratchpadAction myScratchPads "largeTerminal"
    | askToKill appn role  = bindOn' killPrompt
    | otherwise = bindOn' $ killWindow win

killPrompt :: X ()
killPrompt = withFocused $ \win -> do
  pal <- io readPalette
  runSelectedAction (myGsconfig pal)
        [ ("Kill active window", killWindow win)
        , ("Cancel", return ())
        ]

killPrompt' :: X ()
killPrompt' = withFocused $ \w -> do
  pal <- io readPalette
  runSelectedAction (myGsconfig pal)
        [ ("Cancel", return ())
        , ("Kill active window", killWindow w)
        , ("Run xkill", spawn "xkill")
        , ("Kill ALL workspace windows", killAll)
        ]




-- | View a workspace by name and maybe run workspace action
myViewWS wsid = do
  showWorkspaceNameOld
  DW.addHiddenWorkspace wsid
  windows (W.view wsid)
  maybeWorkspaceAction
  updateStruts
  showWorkspaceNameFast

afterWSPrompt w = do s <- gets windowset
                     showWorkspaceNameOld
                     if W.tagMember w s
                       then windows $ W.view w
                       else DW.addWorkspace w >> updateStruts
                     showWorkspaceNameFast




-- | Run script with same name as "w.workspacename"
workspaceAction = do
  ws <- gets (W.currentTag . windowset)
  safeSpawn "workspace-action" [ws]

-- | Run script with same name as "w.workspacename" if the workspace is empty
maybeWorkspaceAction = onEmptyWorkspace workspaceAction

-- | run something on an empty workspace
onEmptyWorkspace action = do
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) action

-- | durations
showStatusSingleMessageDuration = 0.7
showStatusMultipleMessagesDuration = 0.7

-- | Show active workspace name slow
showWorkspaceName = showWorkspaceName' showStatusMultipleMessagesDuration ("bg_0", "yellow")
-- | Show inactve workspace name slow
showWorkspaceNameOld = showWorkspaceName' showStatusMultipleMessagesDuration ("fg_1", "bg_2")
-- | Show active workspace name fast
showWorkspaceNameFast = showWorkspaceName' showStatusSingleMessageDuration ("bg_0", "magenta")

showAppName = showMessage 1 ("bg_0", "cyan")

getScreenRect :: X Rectangle
getScreenRect = screenRect . W.screenDetail . W.current <$> gets windowset

-- | Show msg in a dzen popup in the middle of the screen, in the colors
-- (fg, bg) of the palette: bg_0 text on an accent reads in both modes.
showMessage timeout (fgName, bgName) msg = do
  pal <- io readPalette
  let (fg, bg) = (col pal fgName, col pal bgName)
  sr <-  getScreenRect
  let rx p = fromIntegral $ p sr
      sw = rx rect_width
      sh = rx rect_height
      sx = rx rect_x
      sy = rx rect_y
      w = max (sw/3) (min sw 400) :: Double
      x = sx + sw/2 - w/2
      h = min sh 100 :: Double
      y = sy + sh/2 - h/2

  DZ.dzenConfig
    (DZ.timeout timeout
     >=> DZ.addArgs ["-x", show x, "-w", show w]
     >=> DZ.addArgs ["-y", show y, "-h", show h]
     >=> DZ.font largeFont
     >=> DZ.addArgs ["-fg", fg]
     >=> DZ.addArgs ["-bg", bg]
    ) msg


-- | Show workspace name
showWorkspaceName' timeout colors = do
  wsn <- gets (W.currentTag . windowset)
  showMessage timeout colors wsn

-- | Show current layout name
showLayoutName = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset
  showMessage 2 ("bg_0", "green") ld


-- | Enforce recaluclation of docks gaps. After updating a ~5month old xmonad master today docks were not being avoided by avoudStruts on new desktop.
updateStruts = docksStartupHook

-- Local Variables:
-- fill-column: 165
-- shell-command-after-save-cmd: "xmonad --recompile && xmonad --restart"
-- End:
