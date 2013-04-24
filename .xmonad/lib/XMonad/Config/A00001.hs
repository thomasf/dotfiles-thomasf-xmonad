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
-- Stability   :  unstable
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

import System.Directory (doesFileExist)
import           Control.Monad
import           Data.Ratio ((%))
import           System.IO
import qualified System.IO.UTF8
import           XMonad hiding ( (|||), Tall )
import           XMonad.Actions.CycleWS hiding (toggleWS)
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer    (gotoMenuArgs)
import qualified XMonad.Config.Desktop as Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops       (ewmh)
import           XMonad.Hooks.ManageDocks as MD
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.HintedTile
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.Layout.MultiToggle.Instances as MTI
import           XMonad.Layout.Renamed
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import qualified XMonad.Layout.Spiral as Spiral
import           XMonad.Prompt hiding (height)
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet                 as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.Dzen as DZ
import qualified Solarized as Sol
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.Environment (getEnv)
import XMonad.Actions.CycleRecentWSAddons


------------------------------------------------------------------------
-- Keyboard configuration:

-- simply for convenience and readability
confModMask = mod4Mask

-- align-regexp rules: "addName", "\$"
myKeys conf =
  ((subtitle "....":) $ mkNamedKeymap conf $
  [ ("M-<F2>",   addName "friends browser desktop.."                       $ myViewWS' "friends")
  ]) ++
  ((subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]":) $ mkNamedKeymap conf $
  [ ("M-j",             addName "Focus next window on workspace"                       $ windows W.focusDown >> movePointer)
  , ("M-k",             addName "Focus previous window on workspace"                   $ windows W.focusUp >> movePointer)
  , ("M-C-j",           addName "Swap focused with next on workspace"                  $ windows W.swapDown >> movePointer)
  , ("M-C-k",           addName "Swap focused with previous on workspace"              $ windows W.swapUp >> movePointer)
  , ("M-S-j",           addName "Swap focused with next on workspace"                  $ windows W.swapDown >> movePointer)
  , ("M-S-k",           addName "Swap focused with previous on workspace"              $ windows W.swapUp >> movePointer)
  ]) ++
  ((subtitle "Application launching.":) $ mkNamedKeymap conf $
  [ ("M-o s",   spawn' "sshmenu")
  , ("M-o p",   spawn' "appmenu")
  , ("M-o M-p", spawn' "xfce4-appfinder")
  , ("M-o w",   spawn' "www")
  , ("M-o d",   spawn' "www-dev")
  , ("M-o t",   spawn' "urxvt")
  , ("M-o n",   spawn' "nautilus")
  , ("M-o h",   spawn'  "zeal")
  , ("M-o a",   addName "Run default workspace launcer script"                 $ workspaceAction)
  ]) ++
  ((subtitle "Other window actions":) $ mkNamedKeymap conf $
  [ ("M-<Return>",        addName "Swap the focused window and the master window"        $ windows W.swapMaster >> movePointer)
  , ("M-t",             addName "Push the window into tiling mode"                     $ withFocused (windows . W.sink) >> movePointer)
  , ("M-C-c",           addName "kill"                                                 $ kill)
  , ("M-u",             addName "Focus urgent winow"                                   $ focusUrgent >> movePointer >> showWorkspaceName)
  , ("M-C-u",           addName "Clear all urgent window statuses"                     $ clearUrgents)
  , ("M-y",             addName "Goto workspace by window search prompt"               $ gotoMenuArgs ["-l", "48"] >> movePointer >> showWorkspaceName)
  ]) ++
  ((subtitle "Cyclic display actions (D/F) [+=select] [+control=swap] [+shift=move window to]":) $ mkNamedKeymap conf $
  [ ("M-d",             addName "Next screen"                                          $ nextScreen >> movePointer >> showWorkspaceNameFast)
  , ("M-f",             addName "Previous screen"                                      $ prevScreen >> movePointer >> showWorkspaceNameFast)
  , ("M-C-d",           addName "Swap current display witn next"                       $ swapNextScreen >> showWorkspaceNameOld >> nextScreen >> movePointer >> showWorkspaceName)
  , ("M-C-f",           addName "Swap current display witn previous"                   $ swapPrevScreen >> showWorkspaceNameOld >> nextScreen >> movePointer >> showWorkspaceName)
  , ("M-S-d",           addName "Move window to next screen"                           $ shiftNextScreen >> nextScreen >> movePointer >> showWorkspaceNameFast)
  , ("M-S-f",           addName "Move window to previous screen"                       $ shiftPrevScreen >> prevScreen >> movePointer >> showWorkspaceNameFast)
  ]) ++
  ((subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]":) $ mkNamedKeymap conf $
  [ ("M-e",             addName "Next workspace (prefix)"                              $ rmEmptyWs $ nextWsPrefix >> movePointer >> showWorkspaceName)
  , ("M-r",             addName "Previous workspace (prefix)"                          $ rmEmptyWs $ prevWsPrefix >> movePointer >> showWorkspaceName)
  , ("M-S-<Space>", addName "reset layout" $ setLayout $ XMonad.layoutHook conf)
  ]) ++
  ((subtitle "Modify current workspace layout... (H/L=size ,.=) [+alt=toggle]":) $ mkNamedKeymap conf $
  [ ("M-C-<Space>",     addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer >> showLayoutName)
  , ("M-M1-<Space>",    addName "Toggle fullscreen"                                    $ sendMessage (MT.Toggle MTI.NBFULL) >> movePointer)
  , ("M-M1-s",          addName "Toggle struts (ignore panels)"                        $ sendMessage ToggleStruts >> movePointer)
  , ("M-M1-b",          addName "Toggle window borders"                                $ sendMessage (MT.Toggle MTI.NOBORDERS) >> movePointer)
  , ("M-h",             addName "Shrink the master area"                               $ sendMessage Shrink >> movePointer)
  , ("M-l",             addName "Expand the master area"                               $ sendMessage Expand >> movePointer)
  , ("M-,",             addName "Increment the number of windows in the master area"   $ sendMessage (IncMasterN 1) >> movePointer)
  , ("M-.",             addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)) >> movePointer)
  ]) ++
  ((subtitle "Multi media keys":) $ mkNamedKeymap conf $
  [ ("<XF86AudioPlay>",   spawn' "mpc toggle")
  , ("<XF86AudioStop",    spawn' "mpc stop")
  , ("S-<XF86AudioPrev>", spawn' "mpc prev")
  , ("S-<XF86AudioNext>", spawn' "mpc next")
  , ("<XF86AudioPrev>",   spawn' "mpc seek -00:00:10")
  , ("<XF86AudioNext>",   spawn' "mpc seek +00:00:10")
  , ("<XF86AudioStop>",   spawn' "mpc stop")
 ]) ++
  ((subtitle "misc":) $ mkNamedKeymap conf $
  [ ("M-<Print>",    spawn' "xfce4-screenshooter")
  , ("M-C-<Print>",  addName "scrot focused window" $ safeSpawn "scrot" ["-u", "screenshot-%Y-%m-%d_%H-%M-%S_$wx$h.png", "-e", "mv $f ~/Pictures/scrot/"])
  , ("M-M1-<Print>", addName "scrot full"           $ safeSpawn "scrot" ["screenshot-%Y-%m-%d_%H-%M-%S_$wx$h.png", "-e", "mv $f ~/Pictures/scrot/"])
  , ("M-S-C-c",      spawn' "xkill")
 ]) ++
  ((subtitle "Toggle scratchpads and workspaces":) $ mkNamedKeymap conf $
  [ ("M-<Space>",           addName "Show larger terminal pad"                           $ largeTerminalPad >> movePointer)
  , ("M-i b",               addName "Show vbo workspace"                                 $ myViewWS' "vbox")
  , ("M-i c",               addName "Show chat workspace"                                $ myViewWS' "chat")
  , ("M-i d",               addName "Show dashboard workspace"                           $ myViewWS' "dash")
  , ("M-i f",               addName "Show files workspace"                               $ myViewWS' "files")
  , ("M-i h",               addName "Show home workspace"                                $ myViewWS' "home")
  , ("M-i m",               addName "Show mail workspace"                                $ myViewWS' "mail")
  , ("M-i n",               addName "Show nodes workspace"                               $ myViewWS' "nodes")
  , ("M-i r",               addName "Show read workspace"                                $ myViewWS' "read")
  , ("M-i s",               addName "Show scratch workspace"                             $ myViewWS' "scratch")
  , ("M-i v",               addName "Show video workspace"                               $ myViewWS' "video")
  , ("M-i w",               addName "Show www workspace"                                 $ myViewWS' "www")
  , ("M-i M-i",             addName "cycle ws"                                           $ rmEmptyWs $ myCycleRecentWs xK_i xK_o)
  , ("M-i <Space> <Space>", addName "Create or change workspace prompt"                  $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer >> showWorkspaceName)
  , ("M-i <Space> m",       addName "Move window to other workspace prompt"              $ DW.withWorkspace myXPConfig (windows . W.shift) >> movePointer >> showWorkspaceName)
  , ("M-i <Space> r",       addName "Rename current workspace"                           $ DW.renameWorkspace myXPConfig >> movePointer >> showWorkspaceName)
  , ("M-i <Space> 0",       addName "Remove current workspace"                           $ DW.removeWorkspace >> movePointer >> showWorkspaceName)
 ]) ++
  ((subtitle "exit/quit/leave/reboot...":) $ mkNamedKeymap conf $
  [ ("M-q r",             addName "restart xmonad"                       $ restart "xmonad" True)
  , ("M-q x x x",         addName "restart xmonad without keeping state" $ restart "xmonad" False)
  , ("M-q k k k",         addName "KILL xmonad"                          $ io $ exitWith ExitSuccess)
  , ("M-q <Space> h h h", addName "hibernate computer"                   $ spawn "a.hibernate")
  , ("M-q <Space> s s s", addName "suspend computer"                     $ spawn "a.suspend")
  , ("M-q <Space> p p p", addName "power off computer"                   $ spawn "a.shutdown")
  , ("M-q <Space> l l l", addName "leave computer"                       $ spawn "a.leave")
  , ("M-q <Space> r r r", addName "reboot computer"                      $ spawn "a.reboot")
 ])
  where
    -- | Move mouse pointer to bottom right of the current window
    movePointer = updatePointer (Relative 0.99 0.99)

    -- | Run script with same name as "w.workspacename"
    workspaceAction = do
      ws <- gets (W.currentTag . windowset)
      safeSpawn ("w." ++ takeWhile (/='.') ws) [ ]

    -- | Run script with same name as "w.workspacename" if the workspace is empty
    maybeWorkspaceAction = do
      wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
      when (null wins) $ workspaceAction

    -- | Remove current workpace if empty
    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept [ "NSP" ]

    -- | View a workspace by name and maybe run workspace action
    myViewWS wsid = do
      DW.addHiddenWorkspace wsid
      windows (W.greedyView wsid)
      maybeWorkspaceAction

    -- | View a workspace by name, remove left over empty workspace and move pointer
    myViewWS' wsid = do
      rmEmptyWs $ myViewWS wsid
      movePointer
      showWorkspaceNameFast

    -- | Select workspae prompt
    selectWorkspacePrompt = workspacePrompt myXPConfig $ \w ->
                            do s <- gets windowset
                               if W.tagMember w s
                                 then windows $ W.view w
                                 else DW.addWorkspace w

    -- | Open larger terminal pad
    largeTerminalPad = namedScratchpadAction myScratchPads "largeTerminal"

    -- |  Select next workspace with same prefix
    nextWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Next (HiddenWSTagGroup '.') 1

    -- | Select previous workspac with same prefix
    prevWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Prev (HiddenWSTagGroup '.') 1


    -- | CycleRecentWs that does not include visible but non-focused workspaces or NSP
    cycleRecentWS' = cycleWindowSets' options
     where options w = map (W.view `flip` w) (recentTags w)
           recentTags w = filterNSP map W.tag $ W.hidden w ++ [W.workspace (W.current w)]
           filterNSP = fmap (.namedScratchpadFilterOutWorkspace)

    -- | Cycle recent ws
    myCycleRecentWs keyForward keyBackward = cycleRecentWS'
                                             [ xK_Alt_L, xK_Alt_R
                                             , xK_Super_L, xK_Super_R
                                             , xK_Hyper_L, xK_Hyper_R
                                             , xK_Control_L, xK_Control_R]
                                               keyForward keyBackward showWorkspaceNameFast

    -- | Sort workspaces by tag name, exclude hidden scrachpad workspace.
    getSortByTagNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByTag

-- | Colors
myNormalColor  = Sol.green
myFocusedColor = Sol.magenta
myUrgentColor = Sol.blue
myUrgentColor2bg = Sol.blueL
myUrgentColor2fg = Sol.blueD
myNormalBorderColor darkmode = if darkmode then Sol.base02 else Sol.base2

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
  Desktop.desktopLayoutModifiers $ -- < only implies avoidStruts (ons jul 18 08:22 2012)
  MT.mkToggle (MT.single MTI.NOBORDERS) $
  MT.mkToggle (MT.single MTI.NBFULL) $
  onWorkspace "chat" (renameStar gridWide) $
  onWorkspace "music" (tabs) $
  onWorkspace "files" (grid ||| tabs) $
  onWorkspace "home" (tabs ||| grid) $
  onWorkspace "nodes" (renameStar tabs) $
  onWorkspace "read" (renameStar tabs) $
  onWorkspace "dash" (dash ||| grid) $
  lessBorders OnlyFloat $
  (wide ||| tabs ||| gridWide ||| spiral)
  where
    rename name' = renamed [Replace name']
    renameStar = renamed [Replace "*"]
    full = rename "full" $ noBorders (fullscreenFull Full)
    wide = rename "wide" $ HintedTile 2 (3/100) (4/5) Center Wide
    dash = rename "dash" $ HintedTile 1 0 0.6 Center Wide
    spiral = rename "spiral" $ Spiral.spiral (6/7)
    tabs = rename "tabs" $ HintedTile 1 0 0.93 Center Wide
    gridWide = rename "grid" $ GridRatio (16/9) False
    grid = rename "grid" $ GridRatio (4/3) False

-----------------------------------------------------------------------
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
  fullscreenManageHook <+>  namedScratchpadManageHook myScratchPads <+> (composeOne . concat $
  [ [resource  =? r -?>                                        doIgnore           | r <- ["Do", "desktop_window", "kdesktop"]]
  , [className =? c -?>                                        doIgnore           | c <- ["Unity-2d-panel", "Xfce4-notifyd", "Xfdesktop"]]
  , [className =? c -?>                                        doSink             | c <- ["emulator64-mips", "emulator-arm", "emulator-x86"
                                                                                    ,"emulator64-arm", "emulator64-x86", "emulator-mips"]]
  , [resource  =? r -?>                                        doFloat            | r <- ["speedbar", "floating"]]
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
  ]) <+> manageHook Desktop.desktopConfig -- < implies only manageDocks (ons jul 18 08:51 2012)
  where
    doCenterFloatLarge = customFloating $ W.RationalRect left top width height
      where
        width = 0.6
        height = 0.8
        left = (1 - width) / 2
        top = (1 - height) / 2
    doSink = ask >>= doF . W.sink
    role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Event handling

-- Defines a <custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = serverModeEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myXmobarPP = defaultPP
  { ppCurrent = xmobarColor myFocusedColor "" . wrap "-" "-"
  , ppVisible = xmobarColor myNormalColor "" . wrap " " " "
  , ppHidden  = const ""
  , ppUrgent  = xmobarColor myUrgentColor2fg myUrgentColor2bg . wrap " !*" "*! "
  , ppTitle   = xmobarColor Sol.yellow "" .  trim
  , ppLayout  = const ""
  , ppSep     = xmobarColor Sol.yellow "" " :: "
  , ppSort    = getSortByXineramaRule
  }

myPropLogHook = dynamicLogString myXmobarPP >>= xmonadPropLog

myLogHook = do
  myPropLogHook
  workspaceHistoryHook

------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
  return ()

------------------------------------------------------------------------
-- Urgency hook

myUrgencyHook =
  withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = myUrgentColor }
    urgencyConfig
      { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }


------------------------------------------------------------------------
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


------------------------------------------------------------------------
-- Scratch pads:

myScratchPads = [ NS "largeTerminal" (term "largeTerminal") (res =? scratch "largeTerminal") largeCenterFloat
                ]
  where
    scratch sname = "scratchpad_" ++ sname
    term sname = myTerminal ++ " -name scratchpad_" ++ sname
    -- inTerm' sname scmd = myTerminal ++ " -name scratchpad_" ++ sname ++ " -e " ++  scmd
    res = resource

    largeCenterFloat = customFloating $ W.RationalRect left top width height
      where
        width = 0.8
        height = 0.8
        left = (1 - width) /2
        top = (1 - height) /2

------------------------------------------------------------------------
--  a00001Config

a00001Config = do
  home <- io $ getEnv "HOME"
  darkmode <- doesFileExist $ home ++ "/.config/darkmode"
  return $ myUrgencyHook $ ewmh $ addDescrKeys' ((confModMask, xK_F1), showKeybindings) myKeys $ defaultConfig {
    terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = 2
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor darkmode
  , focusedBorderColor = myFocusedColor
  , keys               = emptyKeys
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , startupHook        = myStartupHook
  , logHook = myLogHook
  } where
    --  | An empty keymap
    emptyKeys c = mkKeymap c [ ]

    -- | Display keyboard mappings using zenity
    showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
    showKeybindings x = addName "Show Keybindings" $ io $ do
      h <- spawnPipe "zenity --text-info --font=terminus"
      System.IO.UTF8.hPutStr h (unlines $ showKm x)
      hClose h
      return ()

-- | Show active workspace name slow
showWorkspaceName = showWorkspaceName' 2.5 Sol.yellow
-- | Show inactve workspace name slow
showWorkspaceNameOld = showWorkspaceName' 2.5 Sol.base1
-- | Show active workspace name fast
showWorkspaceNameFast = showWorkspaceName' 0.8 Sol.magenta

-- | Show workspace name
showWorkspaceName' timeout bg = do
  ws <- gets (W.currentTag . windowset)
  DZ.dzenConfig
    (DZ.timeout timeout
     >=> DZ.onCurr (DZ.center 400 48)
     >=> DZ.font largeFont
     >=> DZ.addArgs ["-fg", Sol.base03]
     >=> DZ.addArgs ["-bg", bg]
    ) ws

-- | Show current layout name
showLayoutName = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset
  DZ.dzenConfig
    (DZ.timeout 0.8
     >=> DZ.onCurr (DZ.center 400 48)
     >=> DZ.font largeFont
     >=> DZ.addArgs ["-fg", Sol.base03]
     >=> DZ.addArgs ["-bg", Sol.green]
    ) ld


-- Local Variables:
-- fill-column: 180
-- End:
