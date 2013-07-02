{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
  FlexibleInstances, MultiParamTypeClasses,
  NoMonomorphismRestriction, ScopedTypeVariables,
  TypeSynonymInstances, UndecidableInstances,
  PostfixOperators #-}
{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports #-}
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
      a00001Config
    ) where


import Data.Maybe ( isJust, catMaybes )
import Codec.Binary.UTF8.String (encodeString)
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.DragPane
import           Control.Monad
import           Data.List
import qualified Data.Map as M
import           Data.Ratio ((%))
import qualified Solarized as Sol
import           System.Directory (doesFileExist)
import           System.Environment (getEnv)
import           System.Exit ( exitSuccess )
import           System.IO
import qualified System.IO.UTF8
import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS hiding (toggleWS)
import           XMonad.Actions.DwmPromote
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.MouseGestures
import qualified XMonad.Actions.Navigation2D as Nav2d
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer    (gotoMenuArgs)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import           XMonad.Layout.BoringWindows hiding (Replace)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import qualified XMonad.Layout.Spiral as Spiral
import           XMonad.Prompt hiding (height)
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet                 as W
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedWindows
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import           XMonad.Util.Paste



-- Keyboard configuration:

-- simply for convenience and readability
confModMask = mod4Mask

myKeys conf =
  subtitle "....": mkNamedKeymap conf
  [ ("M-<F2>",   myViewWS' "friends")
  ] ++
  subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]": mkNamedKeymap conf
  [ ("M-j",             addName "Focus next window on workspace"          $ windows W.focusDown >> movePointer)
  , ("M-k",             addName "Focus previous window on workspace"      $ windows W.focusUp >> movePointer)
  , ("M-n",             addName "Focus next window on workspace"          $ windows W.focusDown >> movePointer)
  , ("M-p",             addName "Focus previous window on workspace"      $ windows W.focusUp >> movePointer)
  , ("M-C-j",           addName "Swap focused with next on workspace"     $ windows W.swapDown >> movePointer)
  , ("M-C-k",           addName "Swap focused with previous on workspace" $ windows W.swapUp >> movePointer)
  , ("M-S-j",           addName "Swap focused with next on workspace"     $ windows W.swapDown >> movePointer)
  , ("M-S-k",           addName "Swap focused with previous on workspace" $ windows W.swapUp >> movePointer)
  ] ++
  subtitle "Application launching": mkNamedKeymap conf
  [ ("M-o o", spawnh "appmenu")
  , ("M-o <Space>",addName "Goto workspace by window search prompt"        $ gotoMenuArgs ["-l", "48"] >> movePointer)
  , ("M-o s", spawnh "sshmenu")
  , ("M-o p", spawnh "xfce4-appfinder")
  , ("M-o w", spawnh "www")
  , ("M-o d", spawnh "www-dev")
  , ("M-o t", spawnh "urxvt")
  , ("M-o e", spawnh "emacs")
  , ("M-o n", spawnh "nautilus")
  , ("M-o z", spawnh  "zeal")
  , ("M-o a", addName "Run default workspace launcer script" workspaceAction)
  , ("M-o v", toggleScratch "pamixer")
  , ("M-o h", toggleScratch "htop")
  ] ++
  subtitle "Other window actions": mkNamedKeymap conf
  [ ("M-<Return>", addName "Swap the focused window and the master window" $ dwmpromote >> movePointer)
  , ("M-t",        addName "Push the window into tiling mode"              $ withFocused (windows . W.sink) >> movePointer)
  , ("M-C-c",      addName "kill"                                            kill)
  , ("M-u",        addName "Focus urgent winow"                            $ focusUrgent >> restoreFocused >> movePointer )
  , ("M-C-u",      addName "Clear all urgent window statuses"                clearUrgents)
  ] ++
  subtitle "Cyclic display actions (D/F) [+=select] [+control=swap] [+shift=move window to]": mkNamedKeymap conf
  [ ("M-f",   addName "Next screen"                        $ nextScreen >> movePointer )
  , ("M-d",   addName "Previous screen"                    $ prevScreen >> movePointer )
  , ("M-C-f", addName "Swap current display witn next"     $ swapNextScreen >> movePointer )
  , ("M-C-d", addName "Swap current display witn previous" $ swapPrevScreen >> movePointer )
  , ("M-S-f", addName "Move window to next screen"         $ shiftNextScreen >> nextScreen >> movePointer )
  , ("M-S-d", addName "Move window to previous screen"     $ shiftPrevScreen >> prevScreen >> movePointer )
  ] ++
  subtitle "2D Navigation": mkNamedKeymap conf
  [ ("M-<Up>",      addName "Focus window above" $ Nav2d.windowGo U False)
  , ("M-<Down>",    addName "Focus window below" $ Nav2d.windowGo D False)
  , ("M-<Left>",    addName "Focus window left"  $ Nav2d.windowGo L False)
  , ("M-<Right>",   addName "Focus window right" $ Nav2d.windowGo R False)
  , ("M-C-<Up>",    addName "Swap window above"  $ Nav2d.windowSwap U False)
  , ("M-C-<Down>",  addName "Swap window below"  $ Nav2d.windowSwap D False)
  , ("M-C-<Left>",  addName "Swap window left"   $ Nav2d.windowSwap L False)
  , ("M-C-<Right>", addName "Swap window right"  $ Nav2d.windowSwap R False)
  ] ++
  subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]": mkNamedKeymap conf
  [ ("M-e",         addName "Next workspace (prefix)"     $ rmEmptyWs $ nextWsPrefix >> movePointer )
  , ("M-r",         addName "Previous workspace (prefix)" $ rmEmptyWs $ prevWsPrefix >> movePointer )
  , ("M-S-<Space>", addName "reset layout"                $ setLayout (XMonad.layoutHook conf) >> movePointer)
  ] ++
  subtitle "Modify current workspace layout... (H/L=size ,.=) [+alt=toggle]": mkNamedKeymap conf
  [ ("M-C-<Space>",  addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer)
  , ("M-g",          addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer)
  , ("M-M1-<Space>", addName "Toggle fullscreen"                                    $ sendMessage (Toggle NBFULL) >> movePointer)
  , ("M-s",          addName "Toggle fullscreen"                                    $ sendMessage (Toggle NBFULL) >> movePointer)
  , ("M-y r",        addName "split screen: right sidebar"                          $ layoutSplitScreen 2 (TwoPane 0 0.85) >> movePointer)
  , ("M-y l",        addName "split screen: left sidebar"                           $ layoutSplitScreen 2 (TwoPane 0 0.15) >> movePointer)
  , ("M-y h",        addName "split screen h"                                       $ layoutSplitScreen 2 (Mirror $ TwoPane 0 (4 /7)) >> movePointer)
  , ("M-y v",        addName "split screen v"                                       $ layoutSplitScreen 2 (TwoPane 0 (5/7)) >> movePointer)
  , ("M-y <Space>",  addName "reset screens"                                        $ rescreen >> movePointer)
  , ("M-M1-s",       addName "Toggle visibiltiy of panels"                          $ sendMessage ToggleStruts >> movePointer)
  , ("M-M1-r",       addName "Toggle reflect layout direction"                      $ sendMessage (Toggle REFLECTX) >> movePointer)
  , ("M-M1-m",       addName "Minimize"                                             $ withFocused minimizeWindow >> movePointer)
  , ("M-M1-u",       addName "UnMinimize"                                           $ sendMessage RestoreNextMinimizedWin)
  , ("M-h",          addName "Shrink the master area"                               $ sendMessage Shrink >> movePointer)
  , ("M-l",          addName "Expand the master area"                               $ sendMessage Expand >> movePointer)
  , ("M-,",          addName "Increment the number of windows in the master area"   $ sendMessage (IncMasterN 1) >> movePointer)
  , ("M-.",          addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)) >> movePointer)
  ] ++
  subtitle "Multi media keys": mkNamedKeymap conf
  [ ("<XF86AudioPlay>",   spawnh "mpc toggle")
  , ("<XF86AudioStop",    spawnh "mpc stop")
  , ("S-<XF86AudioPrev>", spawnh "mpc prev")
  , ("S-<XF86AudioNext>", spawnh "mpc next")
  , ("<XF86AudioPrev>",   spawnh "mpc seek -00:00:10")
  , ("<XF86AudioNext>",   spawnh "mpc seek +00:00:10")
  , ("<XF86AudioStop>",   spawnh "mpc stop")
 ] ++
  subtitle "misc": mkNamedKeymap conf
  [ ("M-<Print>",    spawnh "xfce4-screenshooter")
  , ("M-C-<Print>",  addName "scrot focused window" $ safeSpawn "scrot" ["-u", "screenshot-%Y-%m-%d_%H-%M-%S_$wx$h.png", "-e", "mv $f ~/Pictures/scrot/"])
  , ("M-M1-<Print>", addName "scrot full"           $ safeSpawn "scrot" ["screenshot-%Y-%m-%d_%H-%M-%S_$wx$h.png", "-e", "mv $f ~/Pictures/scrot/"])
  , ("M-S-C-c",      spawnh "xkill")
  , ("<XF86Eject>",  addName "print " $ spawn "xdotool click -clearmodifiers 2")
  , ("M-<XF86Eject>",  addName "print screen" $ sendKey controlMask xK_Print)

  ] ++
  subtitle "Toggle scratchpads and workspaces": mkNamedKeymap conf
  [ ("M-<Space>",           toggleScratch "largeTerminal")
  , ("M-i b",               myViewWS' "vbox")
  , ("M-i c",               myViewWS' "chat")
  , ("M-i d",               myViewWS' "dash")
  , ("M-i f",               myViewWS' "files")
  , ("M-i h",               myViewWS' "home")
  , ("M-i m",               myViewWS' "mail")
  , ("M-i n",               myViewWS' "nodes")
  , ("M-i r",               myViewWS' "read")
  , ("M-i s",               myViewWS' "scratch")
  , ("M-i v",               myViewWS' "video")
  , ("M-i w",               myViewWS' "www")
  , ("M-i M-i",             addName "cycle ws"                              $ rmEmptyWs $ myCycleRecentWs xK_i xK_o)
  , ("M-i i", addName "Create or change workspace prompt"                   $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
  , ("M-i <Space> <Space>", addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
  , ("M-i <Space> m",       addName "Move window to other workspace prompt" $ DW.withWorkspace myXPConfig (windows . W.shift) >> movePointer)
  , ("M-i <Space> r",       addName "Rename current workspace"              $ DW.renameWorkspace myXPConfig >> movePointer)
  , ("M-i <Space> 0",       addName "Remove current workspace"              $ DW.removeWorkspace >> movePointer)
 ] ++
  subtitle "exit/quit/leave/reboot...": mkNamedKeymap conf
  [ ("M-q r",             addName "restart xmonad"                       $ restart "xmonad" True)
  , ("M-q x x x",         addName "restart xmonad without keeping state" $ restart "xmonad" False)
  , ("M-q k k k",         addName "KILL xmonad"                          $ io exitSuccess)
  , ("M-q <Space> h h h", addName "hibernate computer"                   $ spawn "a.hibernate")
  , ("M-q <Space> s s s", addName "suspend computer"                     $ spawn "a.suspend")
  , ("M-q <Space> p p p", addName "power off computer"                   $ spawn "a.shutdown")
  , ("M-q <Space> l l l", addName "leave computer"                       $ spawn "a.leave")
  , ("M-q <Space> r r r", addName "reboot computer"                      $ spawn "a.reboot")
 ]
  where
    spawnh cmd'  = addName cmd' $ spawnHere cmd'
    -- | Move mouse pointer to bottom right of the current window
    movePointer = updatePointer (Relative 0.99 0.99)

    -- | Remove current workpace if empty
    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept [ "NSP", "scratch", "scratch.0" ]

    -- | View a workspace by name and maybe run workspace action
    myViewWS wsid = do
      DW.addHiddenWorkspace wsid
      windows (W.greedyView wsid)
      maybeWorkspaceAction

    -- | View a workspace by name, remove left over empty workspace and move pointer
    myViewWS' wsid = addName("Show " ++ wsid ++ " workspace ") $ do
      rmEmptyWs $ myViewWS wsid
      movePointer

    -- | Select workspae prompt
    selectWorkspacePrompt = workspacePrompt myXPConfig $ \w ->
                            do s <- gets windowset
                               if W.tagMember w s
                                 then windows $ W.view w
                                 else DW.addWorkspace w

    -- | Toggle scratch pad
    toggleScratch cmd' = addName("Toggle " ++ cmd' ++ " scratchpad ") $ namedScratchpadAction myScratchPads cmd'

    -- |  Select next workspace with same prefix
    nextWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Next (HiddenWSTagGroup '.') 1

    -- | Select previous workspac with same prefix
    prevWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Prev (HiddenWSTagGroup '.') 1


    -- | CycleRecentWs that does not include visible but non-focused workspaces or NSP
    cycleRecentWS' = cycleWindowSets options
     where options w = map (W.view `flip` w) (recentTags w)
           recentTags w = filterNSP map W.tag $ W.hidden w ++ [W.workspace (W.current w)]
           filterNSP = fmap (.namedScratchpadFilterOutWorkspace)

    -- | Cycle recent ws
    myCycleRecentWs keyForward keyBackward = cycleRecentWS'
                                             [ xK_Alt_L, xK_Alt_R
                                             , xK_Super_L, xK_Super_R
                                             , xK_Hyper_L, xK_Hyper_R
                                             , xK_Control_L, xK_Control_R]
                                               keyForward keyBackward

    -- | Sort workspaces by tag name, exclude hidden scrachpad workspace.
    getSortByTagNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByTag

    -- | Restore focused window from minimized state
    restoreFocused = withFocused $ \w -> sendMessage (RestoreMinimizedWin w)




-- | Mouse bindings
myMouseBindings =
    [ ((0, button10), mouseGesture gestures) ]
    where
      button10 :: Button
      button10 =  10
      gestures = M.fromList
                 [ ([    ], \w -> focus w)
                 , ([R, D], \_ -> sendMessage NextLayout)
                 , ([L, U], \w -> sendMessage RestoreNextMinimizedWin >> focus w)
                 , ([L, D], \w -> focus w >> withFocused minimizeWindow)
                 , ([U   ], \w -> focus w >> Nav2d.windowSwap U False)
                 , ([D   ], \w -> focus w >> Nav2d.windowSwap D False)
                 , ([L   ], \w -> focus w >> Nav2d.windowSwap L False)
                 , ([R   ], \w -> focus w >> Nav2d.windowSwap R False)
                 ]




-- | Colors
myNormalColor  = Sol.green
myFocusedColor = Sol.magenta
myUrgentColor = Sol.blue
myNormalBorderColor darkmode = if darkmode then Sol.base03 else Sol.base3

-- | Fonts
sizedFont px = "-xos4-terminus-*-r-*-*-" ++ px  ++ "-*-*-*-*-*-iso8859-*"
largeFont = sizedFont "32"



-- | Workspaces
myWorkspaces = [ "scratch", "scratch.0"]
myTerminal = "urxvt"

-- | Layout hook
myLayoutHook =
  onWorkspace "friends" (rename "*friends*" tabs) $
  onWorkspace "video" (renameStar full) $
  onWorkspace "vbox" (renameStar full) $
  -- gaps [(U,32), (D,24)] $
  -- NOTE avoidStuts causes flickering borders and slow down when switching workspaces, gaps are infinitly faster.
  avoidStruts $
  mkToggle (single NBFULL) $
  boringWindows $
  onWorkspace "chat" (renameStar gridWide) $
  onWorkspace "music" tabs $
  onWorkspace "files" (grid ||| tabs) $
  onWorkspace "home" (tabs ||| grid) $
  onWorkspace "nodes" (renameStar tabs) $
  onWorkspace "read" (renameStar tabs) $
  onWorkspace "dash" (dash ||| grid) $
  onWorkspace "im" im $
  lessBorders OnlyFloat
  standard
  where
    standard = wide ||| tall ||| tabs ||| gridWide ||| spiral
    refmin = mkToggle (single REFLECTX) . minimize
    rename name' = renamed [Replace name']
    renameStar = renamed [Replace "*"]
    full = rename "full" $ noBorders (fullscreenFull Full)
    wide = rename "wide" $ Mirror $ refmin $ Tall 2 (3/100) (4/5)
    tall = rename "tall" $ refmin $ Tall 2 (3/100) (3/5)
    dash = rename "dash" $ Mirror $ Tall 1 0 0.6
    spiral = rename "spiral" $ refmin $ Spiral.spiral (6/7)
    tabs = rename "tabs" $ Mirror $ Tall 1 0 0.93
    gridWide = rename "grid" $ refmin $ GridRatio (16/9)
    grid = rename "grid" $ refmin $ GridRatio (4/3)
    im =  withIM (1%9) pidginRoster $ reflectHoriz $ withIM (1%8) skypeRoster
         (gridWide ||| grid ||| spiral)
      where
        pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
        skypeRoster  = (ClassName "Skype")
                       `And` (Not (Title "Options"))
                       `And` (Not (Title "Add a Skype Contact"))
                       `And` (Not (Title "Start a conference call"))
                       `And` (Not (Title "Terms of Use"))
                       `And` (Not (Role "ConversationsWindow"))
                       `And` (Not (Role "CallWindowForm"))
                       `And` (Not (Role "CallWindow"))




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
  manageSpawn <+> fullscreenManageHook <+> namedScratchpadManageHook myScratchPads <+> (composeOne . concat $
  [ [resource  =? r -?>                                        doIgnore           | r <- ["Do", "desktop_window", "kdesktop", "Panel"]]
  , [className =? c -?>                                        doIgnore           | c <- ["Unity-2d-panel", "Xfce4-notifyd", "Xfdesktop"]]
  , [className =? c -?>                                        doSink             | c <- ["emulator64-mips", "emulator-arm", "emulator-x86"
                                                                                         ,"emulator64-arm", "emulator64-x86", "emulator-mips"]]
  , [className =? "Skype" <&&> title =? "Options" -?> doCenterFloatLarge]
  , [className =? "Skype" <&&> startsWith' title "Profile for " -?> doCenterFloat]
  , [className =? "Skype" <&&> startsWith' title "Add a Skype Contact" -?> doCenterFloatLarge]
  , [resource  =? r -?>                                        doFloat            | r <- ["floating"]]
  , [className =? c -?>                                        doFloat            | c <- ["Unity-2d-launcher", "Orage", "feh"]]
  , [role      =? "pop-up" <&&> appName =? "google-chrome" -?> doCenterFloat]
  , [className =? "Zenity" <&&> title =? "Question" -?>        doCenterFloat]
  , [className =? "Zenity" -?>                                 doCenterFloatLarge]
  , [className =? c -?>                                        doCenterFloat      | c <- ["Xfce4-settings-manager", "Pinentry", "connected-app"]]
  , [className =? c -?>                                        doCenterFloatLarge | c <-  ["Xfce4-appfinder"]]
  , [resource  =? c -?>                                        doCenterFloatLarge | c <-  ["floating-center-large"]]
  , [resource  =? c -?>                                        doCenterFloat      | c <-  ["floating-center"]]
  , [transience]
  , [resource  =? "xmessage" -?>                               doCenterFloatLarge]
  , [title     =? "Onboard"  -?>                               doFloat]
  ]) <+> manageDocks
  where
    doCenterFloatLarge = myCenterFloat 0.6 0.8
    doSink = ask >>= doF . W.sink
    role = stringProperty "WM_WINDOW_ROLE"
    startsWith' :: Eq a => Query [a] -> [a] -> Query Bool
    startsWith' q prefix = fmap (isPrefixOf prefix) q



-- Event handling

-- Defines a <custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> minimizeEventHook




-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myXmobarTopPP = defaultPP
  { ppCurrent = xmobarColor myFocusedColor "" . wrap " " " " . trim
  , ppVisible = wrap " " " " . trim
  , ppHidden  = const ""
  , ppUrgent  = xmobarColor myUrgentColor "" . wrap "!"  "!"
  , ppTitle   = const ""
  , ppLayout  = wrap " l:" "" . trim
  , ppSep     = " "
  , ppSort    = getSortByXineramaRule
  }

myXmobarBottomPP = defaultPP
  { ppCurrent = const ""
  , ppVisible = const ""
  , ppHidden  = const ""
  , ppUrgent  = const ""
  , ppTitle   = xmobarColor Sol.magenta ""
  , ppLayout  = const ""
  , ppSep     = xmobarColor Sol.cyan "" " + "
  , ppSort    = getSortByXineramaRule
  }

myLogHook = do
  dynamicLogString myXmobarTopPP >>= xmonadPropLog' "_XMONAD_LOG_TOP"
  dynamicLogString myXmobarBottomPP >>= xmonadPropLog' "_XMONAD_LOG_BOTTOM"
  workspaceHistoryHook
  ewmhDesktopsLogHook
  setWMName "LG3D"



-- Startup hook

myStartupHook = do
  ewmhDesktopsStartup
  setWMName "LG3D"
  return ()



-- Urgency hook

myUrgencyHook =
  withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = myUrgentColor }
    urgencyConfig
      { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }



-- XMonad Prompt configuration

myXPConfig = defaultXPConfig
 { position = Top
 , bgColor = Sol.base03
 , fgColor = Sol.yellow
 , bgHLight = Sol.yellow
 , fgHLight = Sol.base03
 , promptBorderWidth = 0
 , font = "-xos4-terminus-*-r-*-*-16-*-*-*-*-*-iso8859-*"
 , promptKeymap = emacsLikeXPKeymap }



-- Scratch pads:

myScratchPads = [ NS "largeTerminal" (term "largeTerminal") (res =? scratch "largeTerminal") $ myCenterFloat 0.8 0.8
                , termScratch "pamixer" $ myCenterFloat 0.7 0.2
                , termScratch "htop" $ myCenterFloat 0.9 0.9
                ]
  where
    scratch sname = "scratchpad_" ++ sname
    term sname = myTerminal ++ " -name " ++ scratch sname
    termScratch scmd = NS scmd (inTerm' scmd scmd) (res =? scratch scmd)
    inTerm' sname scmd = term sname ++ " -e " ++  scmd
    res = resource


myCenterFloat w h = customFloating $ W.RationalRect left top width height
  where
    width = w
    height = h
    left = (1 - width) / 2
    top = (1 - height) / 2



--  a00001Config

a00001Config = do
  home <- io $ getEnv "HOME"
  darkmode <- doesFileExist $ home ++ "/.config/darkmode"
  return $ myUrgencyHook $ addDescrKeys' ((confModMask, xK_F1), showKeybindings) myKeys $ defaultConfig {
    terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = 4
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor darkmode
  , focusedBorderColor = myFocusedColor
  , keys               = emptyKeys
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , startupHook        = myStartupHook
  , logHook = myLogHook
  } `additionalMouseBindings` myMouseBindings
   where
    --  | An empty keymap
    emptyKeys c = mkKeymap c [ ]

    -- | Display keyboard mappings using zenity
    showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
    showKeybindings x = addName "Show Keybindings" $ io $ do
      h <- spawnPipe "zenity --text-info --font=terminus"
      System.IO.UTF8.hPutStr h (unlines $ showKm x)
      hClose h
      return ()



-- Utilities:

-- | Run script with same name as "w.workspacename"
workspaceAction = do
  ws <- gets (W.currentTag . windowset)
  safeSpawn ("w." ++ takeWhile (/='.') ws) [ ]

-- | Run script with same name as "w.workspacename" if the workspace is empty
maybeWorkspaceAction = do
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) workspaceAction

-- Local Variables:
-- fill-column: 180
-- End:

