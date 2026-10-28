{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
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
import           Control.Monad
import           Data.Char (isSpace)
import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           System.Exit ( exitSuccess )
import           System.IO
import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleRecentWSAddons
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseGestures
import qualified XMonad.Actions.Navigation2D as Nav2d
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.Warp as Warp
import qualified XMonad.Actions.AdjacentScreens as AdjSc
import qualified XMonad.Actions.PrefixWorkspaces as PW
import           XMonad.Actions.WithAll
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook hiding (args)
import           XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import qualified XMonad.Hooks.WorkspaceIPC as WsIPC
import           XMonad.Layout.BoringWindows hiding (Replace, swapUp, swapDown)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.TwoPane
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.LayoutScreens as LS
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.EdgeSpacing as ES
import           XMonad.Layout.IfLayout (ifLayout)
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import qualified XMonad.Layout.PhysicalFullscreen as PF
import qualified XMonad.Layout.ScreenArrangement as SA
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.SimpleFloat (simpleFloat)
import qualified XMonad.Layout.Spiral as Spiral
import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Subsequence as Subseq
import qualified XMonad.StackSet as W hiding (swapUp, swapDown)
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Paste
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.ThemePalette as TP

-- Keyboard configuration:

-- simply for convenience and readability
confModMask = mod4Mask

myKeys host xpc conf=
  subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]": mkNamedKeymap conf
  [ ("M-j",             addName "Focus next window on workspace"          _windowFocusDown)
  , ("M-k",             addName "Focus previous window on workspace"      _windowFocusUp)
  , ("M-C-j",           addName "Swap focused with next on workspace"     _windowSwapDownKeepFocus)
  , ("M-C-k",           addName "Swap focused with previous on workspace" _windowSwapUpKeepFocus)
  , ("M-S-j",           addName "Swap focused with next on workspace"     _windowSwapDown)
  , ("M-S-k",           addName "Swap focused with previous on workspace" _windowSwapUp)
  ] ++
  subtitle "Application launching": mkNamedKeymap conf
  [ ("M-o o",       spawnh   "appmenu")
  , ("M-o <Space>", addName  "Goto workspace by window search (ws window)"   $ spawn "ws window")
  , ("M-o c",       spawnhm  "google-chrome")
  , ("M-o f",       spawnhm  "firefox")
  , ("M-o u",       spawnh'  "x.uptime")
  , ("M-o i",       spawnh'  "x.info")
  , ("M-o j",       spawnh'  "x.todo")
  , ("M-o w",       spawnhm  "www")
  , ("M-o d",       spawnhm  "www-dev")
  , ("M-o t",       spawnh   "term")
  , ("M-o S-t",     spawnh   "env WSENV_SKIP=1 term")
  , ("M-o e",       spawnhmw "e")
  , ("M-o n",       spawnhm  "nautilus")
  , ("M-o a",       addName  "Run default workspace launcher script" $ bindOnProtectedWorkspace workspaceAction maybeWorkspaceAction)
  ] ++
  subtitle "Other window actions": mkNamedKeymap conf
  [ ("M-<Return>", addName "Swap the focused window and the master window" $ dwmpromote >> movePointer)
  , ("M-t",        addName "Push the window into tiling mode"              $ withFocused (windows . W.sink) >> movePointer)
  , ("M-C-c",      addName "kill"                                          kill')
  , ("M-S-C-c",    addName "run xkill or kill all windows on special workspaces" killPrompt')
  , ("M-u",        addName "Focus urgent window"                           $ focusUrgent >> movePointer )
  , ("M-C-u",      addName "Clear all urgent window statuses"              $ clearUrgents >> focusUrgent)
  ] ++
  subtitle "Horizontal screen actions": mkNamedKeymap conf
  [ ("M-f",   addName "Next screen"                        $ showWorkspaceNameOld >> PF.physicalScreenGo R True >> movePointer >> showWorkspaceNameFast)
  , ("M-d",   addName "Previous screen"                    $ showWorkspaceNameOld >> PF.physicalScreenGo L True >> movePointer >> showWorkspaceNameFast)
  , ("M-C-f", addName "Swap current display with next"     $ PF.physicalScreenGo R True >> showWorkspaceNameOld >> PF.physicalScreenGo L True >> Nav2d.screenSwap R True >> showWorkspaceNameFast)
  , ("M-C-d", addName "Swap current display with previous" $ PF.physicalScreenGo L True >> showWorkspaceNameOld >> PF.physicalScreenGo R True >> Nav2d.screenSwap L True >> showWorkspaceNameFast)
  , ("M-S-f", addName "Move window to next screen"         $ Nav2d.windowToScreen R True  >> showWorkspaceNameOld >> PF.physicalScreenGo R True >> showWorkspaceNameFast)
  , ("M-S-d", addName "Move window to previous screen"     $ Nav2d.windowToScreen L True  >> showWorkspaceNameOld >> PF.physicalScreenGo L True >> showWorkspaceNameFast)
  ] ++
  subtitle "2D Navigation": mkNamedKeymap conf
  [ ("M-<Up>",        addName "Focus window above" $ AdjSc.focusUp myNavConfig)
  , ("M-<Down>",      addName "Focus window below" $ AdjSc.focusDown myNavConfig)
  , ("M-<Left>",      addName "Focus window left"  $ AdjSc.focusLeft myNavConfig)
  , ("M-<Right>",     addName "Focus window right" $ AdjSc.focusRight myNavConfig)
  , ("M-C-<Up>",      addName "Swap workspace above"  $ AdjSc.screenSwapUp myNavConfig)
  , ("M-C-<Down>",    addName "Swap workspace below"  $ AdjSc.screenSwapDown myNavConfig)
  , ("M-C-<Left>",    addName "Swap workspace left"   $ AdjSc.screenSwapLeft myNavConfig)
  , ("M-C-<Right>",   addName "Swap workspace right"  $ AdjSc.screenSwapRight myNavConfig)
  , ("M-S-<Up>",      addName "Drag window to screen above"  $ AdjSc.windowToScreenUp myNavConfig)
  , ("M-S-<Down>",    addName "Drag window to screen below"  $ AdjSc.windowToScreenDown myNavConfig)
  , ("M-S-<Left>",    addName "Drag window to screen left"   $ AdjSc.windowToScreenLeft myNavConfig)
  , ("M-S-<Right>",   addName "Drag window to screen right"  $ AdjSc.windowToScreenRight myNavConfig)
  , ("M-S-C-<Up>",    addName "Swap window above"  $ AdjSc.windowSwapUp myNavConfig)
  , ("M-S-C-<Down>",  addName "Swap window below"  $ AdjSc.windowSwapDown myNavConfig)
  , ("M-S-C-<Left>",  addName "Swap window left"   $ AdjSc.windowSwapLeft myNavConfig)
  , ("M-S-C-<Right>", addName "Swap window right"  $ AdjSc.windowSwapRight myNavConfig)
  ] ++
  subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]": mkNamedKeymap conf
  [ ("M-e",         addName "Next workspace (prefix)"     $ rmEmptyWs $ nextWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-r",         addName "Previous workspace (prefix)" $ rmEmptyWs $ prevWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-C-e",       addName "New workspace in prefix.sequence" $ newPrefixWS >> movePointer >> showWorkspaceNameFast)
  , ("M-S-<Space>", addName "reset layout"                $ setLayout (XMonad.layoutHook conf) >> movePointer)
  ] ++
  subtitle "Modify current workspace layout... (H/L=size ,.=) [+alt=toggle]": mkNamedKeymap conf
  [ ("M-C-<Space>", addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer >> showLayoutName)
  , ("M-m",         addName "Toggle fullscreen"                                    $ sendMessage (Toggle NBFULL) >> movePointer >> showLayoutName)
  , ("M-S-m",       addName "Toggle fullscreen across physical screen"             $ PF.toggleFullPhysicalScreen >> movePointer)
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
  [ ("M-5 5",  addName "Standard screen arrangement for host"             $ standardScreenArrangement host >> movePointer)
  , ("M-5 2",  addName "Split current screen by 2 panes 0.5"              $ LS.layoutSplitScreen 2 $ ES.innerSpacing 1 (TwoPane 0 0.5))
  , ("M-5 1",  addName "Split current screen by 2 panes 0.7"              $ LS.layoutSplitScreen 2 $ ES.innerSpacing 1 (TwoPane 0 0.33))
  , ("M-5 3",  addName "Split current screen by 3 even columns"           $ LS.layoutSplitScreen 3 $ ES.innerSpacing 1 (Mirror (Tall 3 0 (1/2))))
  , ("M-5 4",  addName "Split current screen by 3 larger center"          $ LS.layoutSplitScreen 3 $ ES.innerSpacing 1 $ (ThreeColMid 1 (3/100) (4/10)))
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
  , ("M-i M-i",             addName "cycle ws"                              $ rmEmptyWs $ PF.resetPhysicalFullscreen >> myCycleRecentWs xK_i xK_o)
  , ("M-i i",               addName "Create or change workspace (ws pick)"  $ spawn "ws pick -go")
  -- , ("M-i i",               addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePromptHidden >> maybeWorkspaceAction >> movePointer)
  -- , ("M-C-i M-C-i",         addName "cycle ws on next screen"               $ holdScreenFocus $ nextScreen' >> myCycleRecentWs xK_i xK_o)
  -- , ("M-C-u M-C-u",         addName "cycle ws on prev screen"               $ holdScreenFocus $ prevScreen' >> myCycleRecentWs xK_o xK_i)
  , ("M-i <Space> <Space>", addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
  , ("M-S-i",               addName "Move window to other workspace (ws pick -shift)" $ spawn "ws pick -shift")
  , ("M-i <Space> r",       addName "Rename current workspace (ws rename)"  $ spawn "ws rename")
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

    -- | Select workspace prompt
    selectWorkspacePrompt = wsPrompt workspacePromptAll

    -- | Prompt for any workspace, sorted by index
    workspacePromptAll :: XP.XPConfig -> (String -> X ()) -> X ()
    workspacePromptAll c job = do ws <- gets (W.workspaces . windowset)
                                  s <- getSortByIndex
                                  XP.mkXPrompt (Subseq.WsPrompt "") c (XP.mkComplFunFromList' c (map W.tag $ s ws)) job

    -- | Prompt for a workspace, created hidden if it is new, and run job on it
    withWorkspacePrompt :: XP.XPConfig -> (String -> X ()) -> X ()
    withWorkspacePrompt c job = workspacePromptAll c $ \t -> do
      exists <- gets (W.tagMember t . windowset)
      unless exists $ DW.addHiddenWorkspace t
      job t

    -- | Toggle scratch pad
    toggleScratch cmd' = addName("Toggle " ++ cmd' ++ " scratchpad ") $ namedScratchpadAction myScratchPads cmd'

    wsSeparator = PW.defaultWsSeparator

    gotoBaseWS :: X ()
    gotoBaseWS = PF.resetPhysicalFullscreen >> PW.gotoBaseWSWith wsSeparator myViewWS

    gotoPrefixWS :: String -> X ()
    gotoPrefixWS suffix = PF.resetPhysicalFullscreen >> PW.gotoPrefixWSWith wsSeparator myViewWS suffix

    worktreePrompt :: X ()
    worktreePrompt = PW.worktreePromptWith wsSeparator xpc myViewWS

    nextWsPrefix = PW.nextWsPrefixWith wsSeparator getSortByTagNoSP

    prevWsPrefix = PW.prevWsPrefixWith wsSeparator getSortByTagNoSP

    newPrefixWS :: X ()
    newPrefixWS = PW.newPrefixWSWith wsSeparator myViewWS


    -- | filter some workspaces
    filterSomeWorkspaces = fmap (. filterOutWs cycleSkippedWorkspaces)

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
                 [ ([    ], \_ -> io TP.readPalette >>= \pal -> gridselectWorkspace (myGsconfig pal) W.greedyView)
                 , ([R, D], \_ -> sendMessage NextLayout)
                 , ([U   ], \w -> focus w >> Nav2d.windowSwap U False)
                 , ([D   ], \w -> focus w >> Nav2d.windowSwap D False)
                 , ([L   ], \w -> focus w >> Nav2d.windowSwap L False)
                 , ([R   ], \w -> focus w >> Nav2d.windowSwap R False)
                 ]




-- | Colors

-- | Colors and theme configuration are provided by XMonad.Util.ThemePalette

myDefaultSpacerWidth = 3
myDefaultBorderWidth = 4



-- | Workspaces
myWorkspaces = "s" : baseWorkspaces
myTerminal = "term"

-- | The numbered workspaces there from the start
baseWorkspaces :: [WorkspaceId]
baseWorkspaces = [ "s.0", "s.1", "s.2", "s.3", "s.4" ]

-- | Workspaces kept when they are left empty
keptWorkspaces :: [WorkspaceId]
keptWorkspaces = scratchpadWorkspaceTag : "" : baseWorkspaces

-- | Workspaces where the launcher keys only show the workspace name
protectedWorkspaces :: [WorkspaceId]
protectedWorkspaces = [ "work", "work-client", "dash", "chat", "mail", "home", scratchpadWorkspaceTag, "nodes" ]

-- | Workspaces that recent and prefix workspace cycling skip
cycleSkippedWorkspaces :: [WorkspaceId]
cycleSkippedWorkspaces = [ scratchpadWorkspaceTag, "chat", "dash" ]

-- | Hosts with a tall layout default
tallHosts :: [String]
tallHosts = ["transwhale", "flam", "yak"]

-- | Layout hook
myLayoutHook host =
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
    onTall = ifLayout (host `elem` tallHosts)
    -- helpers
    refmin = mkToggle (single REFLECTX)
    mySpacing = ES.innerSpacing
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
    isTooltip = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLTIP"



-- Event handling

-- Defines a <custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook = PF.fullscreenConfigureHook <+> noRescreenEventHook <+> fullscreenEventHook <+> serverModeEventHook <+> wsIPCEventHook


noRescreenEventHook ConfigureEvent {ev_window = w} = do
    r <- asks theRoot
    return $ All $ w /= r

noRescreenEventHook _ = mempty

-- | Client messages from the "ws" command.
wsIPCEventHook :: Event -> X All
wsIPCEventHook = WsIPC.wsIPCEventHook WsIPC.WsIPCConfig
  { WsIPC.wsIPCGo     = wsGo
  , WsIPC.wsIPCShift  = wsShift
  , WsIPC.wsIPCRename = wsRename
  , WsIPC.wsIPCFocus  = wsFocus
  }

-- | Go to the workspace "ws go" names, as the workspace prompt did. A
-- new workspace is created, an empty one gets its workspace action, and
-- the workspace left is removed if empty. EWMH can only switch to
-- existing workspaces.
wsGo :: WorkspaceId -> X ()
wsGo n = do
  PF.resetPhysicalFullscreen
  DW.removeEmptyWorkspaceAfterExcept keptWorkspaces $ do
    exists <- gets (W.tagMember n . windowset)
    if exists
      then showWorkspaceNameOld >> windows (W.view n) >> maybeWorkspaceAction >> showWorkspaceNameFast
      else myViewWS n
    movePointer

-- | Focus the window "ws window" names, as the window search did. Its
-- workspace is shown if need be.
wsFocus :: Window -> X ()
wsFocus w = do
  PF.resetPhysicalFullscreenIfOther w
  windows (W.focusWindow w) >> movePointer

-- | Move the focused window to the workspace "ws shift" / "ws pick -shift" names.
-- Creates the workspace hidden if it is new.
wsShift :: WorkspaceId -> X ()
wsShift n = do
  exists <- gets (W.tagMember n . windowset)
  unless exists $ DW.addHiddenWorkspace n
  windows (W.shift n)
  movePointer

-- | Rename the current workspace to the name "ws rename" gives.
wsRename :: WorkspaceId -> X ()
wsRename n = do
  DW.renameWorkspaceByName n
  movePointer
  showWorkspaceNameFast


-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myXmobarTopPP pal = def
  { ppCurrent = xmobarColor (TP.myFocusedColor pal) "" . wrap " " "" . trim
  , ppVisible = wrap " " "" . trim
  , ppHidden  = const ""
  , ppUrgent  = wrap " ◀"  "▶"
  , ppTitle   = const ""
  , ppLayout  = const ""
  , ppSep     = " "
  , ppSort    = mySortByXineramaPhysicalRule
  }

-- | The bottom bar shows only the focused window's title, sanitized as
-- dynamicLogString would.
myXmobarBottomTitle :: X String
myXmobarBottomTitle = withWindowSet $ \ws -> case W.peek ws of
  Nothing -> return ""
  Just w  -> trim . ppTitleSanitize def . show <$> getName w

newtype PropLogCache = PropLogCache (M.Map String String)
  deriving (Show, Read)

instance ExtensionClass PropLogCache where
  initialValue = PropLogCache M.empty

-- | Emit property log only when the formatted message has changed, preventing
-- unnecessary X11 root PropertyNotify events and redundant xmobar repainting.
xmonadPropLogDedupe :: String -> String -> X ()
xmonadPropLogDedupe prop msg = do
  PropLogCache cache <- XS.get
  case M.lookup prop cache of
    Just prev | prev == msg -> return ()
    _ -> do
      XS.put (PropLogCache (M.insert prop msg cache))
      xmonadPropLog' prop msg

myLogHook pal = do
  dynamicLogString (myXmobarTopPP pal) >>= xmonadPropLogDedupe "_XMONAD_LOG_TOP"
  myXmobarBottomTitle >>= xmonadPropLogDedupe "_XMONAD_LOG_BOTTOM"
  workspaceHistoryHook
  PF.checkPhysicalFullscreenReset
  -- desktopLogHook
  -- fadeInactiveLogHook 0.86


-- | 'logHook' preserving old 'ewmh' behavior to switch workspace and focus to
-- activated window.
-- desktopLogHook :: X ()
-- -- desktopLogHook      = activateLogHook (reader W.focusWindow >>= doF)


-- | Visible workspaces first, ordered by the (x, y) of their screens, then
-- the others by tag. Like getSortByXineramaPhysicalRule, but the screen
-- positions are looked up once per sort, not on every comparison.
mySortByXineramaPhysicalRule :: X WorkspaceSort
mySortByXineramaPhysicalRule = mkWsSort $ do
    w <- gets windowset
    let posMap = M.fromList [ (W.tag (W.workspace s), (rect_x r, rect_y r))
                            | s <- W.current w : W.visible w
                            , let r = screenRect (W.screenDetail s) ]
    return $ \ a b -> case (M.lookup a posMap, M.lookup b posMap) of
        (Just posA, Just posB) -> compare posA posB
        (Nothing,   Nothing)   -> compare a b
        (Just _,    Nothing)   -> LT
        (Nothing,   Just _)    -> GT



-- Startup hook

-- | ewmh runs its startup hook, which sets the WM name to "xmonad", before
-- this one, so LG3D (for Java GUI apps) wins.
myStartupHook = setWMName "LG3D"



-- Urgency hook

myUrgencyHook pal =
  withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = TP.myUrgentColor pal }
    def
      { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }



-- Font configuration:

myFont :: String
myFont = "PragmataPro"

sizedXftFontScaled :: TP.Palette -> Double -> String
sizedXftFontScaled pal px = TP.sizedXftFontScaled pal myFont px

myGsconfig :: TP.Palette -> GSConfig a
myGsconfig = TP.myGsconfigWithFont myFont

showMessage :: (Real a) => a -> (String, String) -> String -> X ()
showMessage = TP.showMessageWithFont myFont

-- XMonad Prompt configuration

defXPConfig pal = def
 { XP.position = XP.CenteredAt 0.4 0.5
 , XP.promptBorderWidth = fromIntegral (max 1 (TP.scaleDpi pal 1))
 , XP.font = sizedXftFontScaled pal 20
 , XP.height = fromIntegral (TP.scaleDpi pal 24)
 , XP.promptKeymap = Subseq.emacsLikeXPKeymap' (\c -> isSpace c || c == '.')
 , XP.searchPredicate = Subseq.subsequenceMatch
 , XP.sorter = Subseq.subsequenceSort
 }

-- | Prompts are a raised surface, like rofi: bg_1 with fg_1 text.
myXPConfig pal = (defXPConfig pal)
 { XP.bgColor = TP.col pal "bg_1"
 , XP.fgColor = TP.col pal "fg_1"
 , XP.bgHLight = TP.col pal "bg_1"
 , XP.fgHLight = TP.col pal "magenta"
 , XP.borderColor = TP.col pal "bg_1"
 }



-- Scratch pads:

myScratchPads = [ NS "largeTerminal" (term "largeTerminal") (res =? scratch "largeTerminal") $ myCenterFloat 0.95 0.8
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
  pal <- io TP.readPalette
  host <- SA.getHost
  return . docks . ewmh . Nav2d.withNavigation2DConfig myNavigation2DConfig . myUrgencyHook pal . addDescrKeys' ((confModMask, xK_F1), showKeybindings) (myKeys host $ myXPConfig pal) $ def {
    terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = myDefaultBorderWidth
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = TP.myNormalBorderColor pal
  , focusedBorderColor = TP.myFocusedColor pal
  , keys               = emptyKeys
  , layoutHook         = myLayoutHook host
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

-- | Move pointer to focused window, or to the center of the current screen if no window exists.
focusPointer :: X ()
focusPointer = do
  ws <- gets windowset
  case W.peek ws of
    Just _  -> movePointer
    Nothing -> Warp.warpToScreen (W.screen (W.current ws)) (1/2) (1/2)

-- | 2D screen and window navigation configuration.
myNavConfig :: AdjSc.ScreenNavConfig
myNavConfig = AdjSc.defaultScreenNavConfig
  { AdjSc.navBeforeScreenChange = showWorkspaceNameOld
  , AdjSc.navAfterScreenChange  = showWorkspaceNameFast
  , AdjSc.navFocusPointer       = focusPointer
    -- Upper row monitors are above y 1000; on flam the center monitor below
    -- them starts at x 3840, as in standardScreenArrangement.
  , AdjSc.navIsUpperScreen      = \r -> rect_y r < 1000
  , AdjSc.navSingleWindowDown   = Just (\r -> rect_x r >= 3840)
  }

-- | Standard screen splitting arrangement per host.
-- For "flam": leftmost screen split with 0.33 ratio (M-5 1), center screen split with 0.50 ratio (M-5 2).
-- For "yak": single screen split in two with 0.50 ratio (M-5 2).
standardScreenArrangement :: String -> X ()
standardScreenArrangement host = do
  PF.resetPhysicalFullscreen
  SA.arrangeScreens $ case host of
    "flam" -> SA.splitScreenWhere (\r -> rect_x r == 0) 0.33
            . SA.splitScreenWhere (\r -> rect_x r == 3840 && rect_y r >= 1000) 0.50
    "yak"  -> SA.splitSingleScreen 0.50
    _      -> SA.splitSingleScreen 0.50



-- | bind keys but not for some protected workspaces
bindOnProtectedWorkspace cmd' altCmd  = bindOn $
    [ (w, altCmd) | w <- protectedWorkspaces ] ++ [ ("", cmd') ]


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
  pal <- io TP.readPalette
  runSelectedAction (myGsconfig pal)
        [ ("Kill active window", killWindow win)
        , ("Cancel", return ())
        ]

killPrompt' :: X ()
killPrompt' = withFocused $ \w -> do
  pal <- io TP.readPalette
  runSelectedAction (myGsconfig pal)
        [ ("Cancel", return ())
        , ("Kill active window", killWindow w)
        , ("Run xkill", spawn "xkill")
        , ("Kill ALL workspace windows", killAll)
        ]




-- | View a workspace by name and maybe run workspace action
myViewWS wsid = do
  PF.resetPhysicalFullscreen
  showWorkspaceNameOld
  DW.addHiddenWorkspace wsid
  windows (W.view wsid)
  maybeWorkspaceAction
  showWorkspaceNameFast

afterWSPrompt w = do
  PF.resetPhysicalFullscreen
  s <- gets windowset
  showWorkspaceNameOld
  if W.tagMember w s
    then windows $ W.view w
    else DW.addWorkspace w
  showWorkspaceNameFast




-- | Run script with same name as "w.workspacename"
workspaceAction = do
  ws <- gets (W.currentTag . windowset)
  safeSpawn "ws" ["action", ws]

-- | Run script with same name as "w.workspacename" if the workspace is empty
maybeWorkspaceAction = onEmptyWorkspace workspaceAction

-- | run something on an empty workspace
onEmptyWorkspace action = do
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) action

-- | durations
showStatusSingleMessageDuration = 0.7
showStatusMultipleMessagesDuration = 0.7

-- | Show inactve workspace name slow
showWorkspaceNameOld = showWorkspaceName' showStatusMultipleMessagesDuration ("fg_1", "bg_2")
-- | Show active workspace name fast
showWorkspaceNameFast = showWorkspaceName' showStatusSingleMessageDuration ("bg_0", "magenta")

showAppName = showMessage 1 ("bg_0", "cyan")


-- | Show workspace name
showWorkspaceName' timeout colors = do
  wsn <- gets (W.currentTag . windowset)
  showMessage timeout colors wsn

-- | Show current layout name
showLayoutName = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset
  showMessage 2 ("bg_0", "green") ld


-- Local Variables:
-- fill-column: 165
-- shell-command-after-save-cmd: "xmonad --recompile && xmonad --restart"
-- End:
