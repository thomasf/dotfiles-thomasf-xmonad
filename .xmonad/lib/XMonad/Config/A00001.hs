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
      autoConfig
    ) where

import Data.Maybe (isJust, catMaybes)
import Data.List
import           Control.Monad
import qualified Data.Map                        as M
import           Data.Ratio ((%))
import           Graphics.X11.Xinerama
import           System.IO
import qualified System.IO.UTF8
import           System.Posix.Unistd             (getSystemID, nodeName)
import           XMonad                          hiding ( (|||) )
import           XMonad.Actions.CycleWS hiding (toggleWS)
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.RotSlaves
import           XMonad.Hooks.DynamicBars
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer    (gotoMenuArgs)
import qualified XMonad.Config.Desktop as Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops       (ewmh)
import           XMonad.Hooks.ManageDocks as MD
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.Decoration
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.Layout.MultiToggle.Instances as MTI
import           XMonad.Layout.Renamed
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.ShowWName
import qualified XMonad.Layout.Spiral as Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet                 as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.NamedWindows
import qualified Solarized as Sol
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import           XMonad.Util.EZConfig
import           XMonad.Operations
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
------------------------------------------------------------------------
-- Keyboard configuration:

-- simply for convenience and readability
confModMask = mod4Mask


-- align-regexp rules: "addName", "\$"
myKeys conf =
 ((subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]":) $ mkNamedKeymap conf $
  [ ("M-j",             addName "Focus next window on workspace"                       $ windows W.focusDown >> movePointer)
  , ("M-k",             addName "Focus previous window on workspace"                   $ windows W.focusUp >> movePointer)
  , ("M-S-j",           addName "Swap focused with next on workspace"                  $ windows W.swapDown >> movePointer)
  , ("M-C-j",           addName "Swap focused with next on workspace"                  $ windows W.swapDown >> movePointer)
  , ("M-S-k",           addName "Swap focused with previous on workspace"              $ windows W.swapUp >> movePointer)
  , ("M-C-k",           addName "Swap focused with previous on workspace"              $ windows W.swapUp >> movePointer)
  ]) ++
  ((subtitle "prefixed testing...":) $ mkNamedKeymap conf $
  [ ("M-o s",   spawn' "sshmenu")
  , ("M-o p",   spawn' "appmenu")
  , ("M-o M-p", spawn' "xfce4-appfinder")
  , ("M-o w",   spawn' "www")
  , ("M-o d",   spawn' "www-dev")
  , ("M-o t",   spawn' "urxvt")
  , ("M-o n",   spawn' "nautilus")
  , ("M-o h",   spawn'  "zeal")
  , ("M-o o",   addName "Goto workspace by window search prompt"               $ gotoMenuArgs ["-l", "48"] >> movePointer >> showWorkspaceName)
  , ("M-o a",   addName "Run default workspace launcer script"                 $ workspaceAction)
  ]) ++
  ((subtitle "Other window actions":) $ mkNamedKeymap conf $
  [ -- ("M-<Space>",       addName "Show some info..."                                    $ showInfo)
  ("M-<Return>",        addName "Swap the focused window and the master window"        $ windows W.swapMaster >> movePointer)
  , ("M-t",             addName "Push the window into tiling mode"                     $ withFocused (windows . W.sink) >> movePointer)
  , ("M-C-c",           addName "kill"                                                 $ kill)
  , ("M-u",             addName "Focus urgent winow"                                   $ focusUrgent >> movePointer >> showWorkspaceName)
  , ("M-C-u",           addName "Clear all urgent window statuses"                     $ clearUrgents)
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
  , ("M-C-M1-e",        addName "Next non empty workspace"                             $ rmEmptyWs $ nextWsNonEmpty >> movePointer >> showWorkspaceName)
  , ("M-C-M1-r",        addName "Previous non empty workspace"                         $ rmEmptyWs $ prevWsNonEmpty >> movePointer >> showWorkspaceName)
  , ("M-M1-e",          addName "New workspace in prefix.sequence"                     $ newPrefixWS >> movePointer >> showWorkspaceName)
  ]) ++
  -- ((subtitle "Other workspace actions":) $ mkNamedKeymap conf $
  -- [ ("M-1",             addName "Goto workspacegroup basename workspace"               $ rmEmptyWs $ gotoPrefixWorkspaceNonSuffix >> showWorkspaceNameFast)
  -- , ("M-2",             addName "Goto workspacegroup .0"                               $ rmEmptyWs $ gotoPrefixWorkspaceSuffix 0 >> showWorkspaceNameFast)
  -- , ("M-3",             addName "Goto workspacegroup .1"                               $ rmEmptyWs $ gotoPrefixWorkspaceSuffix 1 >> showWorkspaceNameFast)
  -- , ("M-4",             addName "Goto workspacegroup .2"                               $ rmEmptyWs $ gotoPrefixWorkspaceSuffix 2 >> showWorkspaceNameFast)
  -- , ("M-5",             addName "Goto workspacegroup .3"                               $ rmEmptyWs $ gotoPrefixWorkspaceSuffix 3 >> showWorkspaceNameFast)
  -- ]) ++
  ((subtitle "Workspace prompts":) $ mkNamedKeymap conf $
  [ ("M-m",             addName "Create or change workspace prompt"                    $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer >> showWorkspaceName)
  , ("M-S-m",           addName "Move window to other workspace prompt"                $ DW.withWorkspace myXPConfig (windows . W.shift) >> movePointer >> showWorkspaceName)
  , ("M-C-m",           addName "Rename current workspace"                             $ DW.renameWorkspace myXPConfig >> movePointer >> showWorkspaceName)
  , ("M-C-<Backspace>", addName "Remove current workspace"                             $ DW.removeWorkspace >> movePointer >> showWorkspaceName)
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
  [ ("M-<Space>", addName "Show larger terminal pad"                           $ largeTerminalPad >> movePointer)
  , ("M-i h",     addName "Show home workspace"                                $ myViewWS3 "home")
  , ("M-1",       addName "Show scratch workspace"                             $ myViewWS3 "scratch")
  , ("M-2",       addName "Show chat workspace"                                $ myViewWS3 "chat")
  , ("M-i n",     addName "Show nodes workspace"                               $ myViewWS3 "nodes")
  , ("M-i m",     addName "Show mail workspace"                                $ myViewWS3 "mail")
  , ("M-0",       addName "Show dashboard workspace"                           $ myViewWS3 "dash")
  , ("M-i v",     addName "Show video workspace"                               $ myViewWS3 "video")
  , ("M-i r",     addName "Show read workspace"                                $ myViewWS3 "read")
  , ("M-i f",     addName "Show files workspace"                               $ myViewWS3 "files")
  , ("M-i M-i",   addName "Toggle previous workspace"                          $ rmEmptyWs $ toggleWS >> showWorkspaceNameFast)
  , ("M-i i",     addName "Toggle previous workspace skipping some workspaces" $ rmEmptyWs $ ignoredToggleWS >> showWorkspaceNameFast)
 ]) ++
  ((subtitle "Quit/restart":) $ mkNamedKeymap conf $
  [ ("M-q r", addName "restart xmonad" $ restart "xmonad" True)
  , ("M-q x", addName "restart xmonad without keeping state" $ restart "xmonad" False)
  , ("M-q k k k", addName "KILL xmonad" $ io $ exitWith ExitSuccess)

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
    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept [ "NSP", "home", "scratch"]

    -- | Toggle recent workspaces ignoring some of them
    toggleWS = toggleWS' [ "NSP" ] >> movePointer

    -- | Toggle recent workspaces ignoring some of them
    ignoredToggleWS = toggleWS' [ "NSP"
                                , "home", "nodes", "dash", "scratch"
                                , "chat", "im" ] >> movePointer

    -- | View a workspace by name and maybe run workspace action
    myViewWS wsid = do
      DW.addHiddenWorkspace wsid
      windows (W.greedyView wsid)
      maybeWorkspaceAction

    -- | View a workspace by name
    myViewWS1 wsid = do
      DW.addHiddenWorkspace wsid
      windows (W.greedyView wsid)

   -- | View a workspace by name and maybe run workspace action
    myViewWS2 wsid = do
      DW.addHiddenWorkspace wsid
      windows (W.view wsid)
      maybeWorkspaceAction

    -- | View a workspace by name, remove left over empty workspace and move pointer
    myViewWS3 wsid = do
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

    -- | Select next non empty workspace
    nextWsNonEmpty = windows . W.greedyView
                     =<< findWorkspace getSortByTagNoSP Next HiddenNonEmptyWS 1

    -- | Select previous non empty workspace
    prevWsNonEmpty = windows . W.greedyView
                     =<< findWorkspace getSortByTagNoSP Prev HiddenNonEmptyWS 1

    -- |  Select next workspace with same prefix
    nextWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Next (HiddenWSTagGroup '.') 1

    -- | Select previous workspac with same prefix
    prevWsPrefix = windows . W.greedyView
                   =<< findWorkspace getSortByTagNoSP Prev (HiddenWSTagGroup '.') 1

    -- | Sort workspaces by tag name, exclude hidden scrachpad workspace.
    getSortByTagNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByTag

    gotoPrefixWorkspaceNonSuffix :: X ()
    gotoPrefixWorkspaceNonSuffix = do
      ws <- gets (W.currentTag . windowset)
      myViewWS2 (takeWhile (/='.') ws)

    gotoPrefixWorkspaceSuffix suffix = do
      ws <- gets (W.currentTag . windowset)
      myViewWS1 ((takeWhile (/='.') ws) ++ "." ++ (show suffix))

    -- | TODO: rewrite
    newPrefixWS :: X ()
    newPrefixWS = withWindowSet $ \w -> do
      thisWS <- gets (W.currentTag . windowset)
      let wss = W.workspaces w
          currentTagPrefix = takeWhile (/='.') thisWS
          cws = map W.tag $ filter (\ws -> (currentTagPrefix ++ ".") `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
          num = head $ [0..] \\ catMaybes (map (readMaybe . drop ((length currentTagPrefix) +1 )) cws)
          new = currentTagPrefix ++ "." ++ (show num)
      when (not $ new `elem` (map W.tag wss)) $ myViewWS new
      windows $ W.view new
        where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing




------------------------------------------------------------------------
-- Basic random
--
myTerminal = "urxvt"
-- myShell = "bash"

------------------------------------------------------------------------
-- Border colors for unfocused and focused windows, respectively.
--
myNormalColor  = Sol.green
myFocusedColor = Sol.magenta
myUrgentColor = Sol.blue

------------------------------------------------------------------------
-- Workspaces

myWorkspaces = [ "scratch", "scratch.0"]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.

sizedFont px = "-xos4-terminus-*-r-*-*-" ++ px  ++ "-*-*-*-*-*-iso8859-*"
defaultFont = sizedFont "13"
largeFont = sizedFont "16"
hugeFont = sizedFont "32"

-- | Base decoration theme
baseTheme = defaultTheme { fontName            = defaultFont
                         , decoHeight          = 24 }

-- | Decoration theme for bottom tabs
bottomTabTheme = baseTheme { activeTextColor     = Sol.base03
                           , activeColor         = myFocusedColor
                           , activeBorderColor   = myFocusedColor
                           , inactiveTextColor   = Sol.base03
                           , inactiveColor       = myNormalColor
                           , inactiveBorderColor = myNormalColor
                           , urgentTextColor     = Sol.base03
                           , urgentColor         = myUrgentColor
                           , urgentBorderColor   = myUrgentColor
                           , decoHeight          = 48 }

-- | The layouthoook
myLayoutHook = onWorkspace "video" (renameStar full) $
               Desktop.desktopLayoutModifiers $ -- < only implies avoidStruts (ons jul 18 08:22 2012)
               MT.mkToggle (MT.single MTI.NOBORDERS) $
               MT.mkToggle (MT.single MTI.NBFULL) $
               onWorkspace "dash" (tabsBottom ||| grid) $
               onWorkspace "chat" (renameStar gridWide) $
               onWorkspace "music" (tabsAlways) $
               onWorkspace "files" (grid ||| tabsAlways) $
               onWorkspace "home" (tabsBottom ||| grid) $
               onWorkspace "nodes" (renameStar tabsBottom) $
               onWorkspace "im" (renameStar im) $
               onWorkspace "read" (renameStar tabsBottom) $
               lessBorders OnlyFloat
               (tallH ||| tallV ||| tabsAlways ||| threeCol |||  threeColV ||| gridWide ||| spiral)
  where
    rename name = renamed [Replace name]
    renameStar = renamed [Replace "*"]
    full = rename "full" $ noBorders (fullscreenFull Full)
    tallH = rename "tall h" $ Mirror $ Tall 2 (3/100) (4/5)
    tallV = rename "tall v" $ Tall 2 (3/100) (1/2)
    -- TODO: these might be usable with a bit of tweaking
    -- tallH = mouseResizableTileMirrored { nmaster  = 1
    --                                    , masterFrac = (4/5)
    --                                    , slaveFrac = (1/2)
    --                                    , fracIncrement = (3/100)
    --                                    , draggerType = BordersDragger }
    -- tallV = mouseResizableTile { nmaster  = 1
    --                            , masterFrac = (3/4)
    --                            , slaveFrac = (1/2)
    --                            , fracIncrement = (3/100)
    --                            , draggerType = BordersDragger }
    spiral = rename "spiral" $ Spiral.spiral (6/7)
    threeCol = rename "3col h" $ ThreeColMid 2 (3/100) (1/2)
    threeColV = rename "3col v" $ Mirror threeCol
    tabsAlways = rename "tabs" $ tabbedBottomAlways shrinkText bottomTabTheme
    tabsBottom = rename "tabs" $ tabbedBottom shrinkText bottomTabTheme
    gridWide =  rename "grid" $ GridRatio (16/9)
    grid = rename "grid" $ GridRatio (4/3)
    im = renameStar $ withIM (1%7) (Role "buddy_list") Grid

-----------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
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
  [ [resource  =? r -?>                                        doIgnore      | r <- ignoreByResource]
  , [className =? c -?>                                        doIgnore      | c <- ignoreByClass]
  , [className =? c -?>                                        doSink        | c <- androidEmulatorByClass]
  , [resource  =? r -?>                                        doFloat       | r <- floatByResource]
  , [className =? c -?>                                        doFloat       | c <- floatByClass]
  , [role      =? "pop-up" <&&> appName =? "google-chrome" -?> doCenterFloat]
  , [className =? "Zenity" <&&> title =? "Question" -?>        doCenterFloat]
  , [className =? "Zenity" -?>                                 doCenterFloatLarge]
  , [className =? c -?>                                        doCenterFloat | c <- centerFloatByClass]
  , [className =? c -?>                                        doCenterFloatLarge | c <- centerFloatLargeByClass]
  , [resource  =? c -?>                                        doCenterFloatLarge | c <- centerFloatLargeByResource]
  , [resource  =? c -?>                                        doCenterFloat | c <- centerFloatByResource]
  , [transience]
  , [resource  =? "xmessage"          -?> doCenterFloatLarge]
  , [title     =? "Onboard"           -?> doFloat]
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
    androidEmulatorByClass =
      ["emulator64-mips", "emulator-arm", "emulator-x86"
      ,"emulator64-arm", "emulator64-x86", "emulator-mips"]
    ignoreByResource =
      ["Do", "desktop_window", "kdesktop"]
    ignoreByClass =
      ["Unity-2d-panel", "Xfce4-notifyd", "Xfdesktop"]
    floatByResource =
      ["speedbar", "floating"]
    floatByClass =
      ["Unity-2d-launcher", "Orage", "feh"]
    centerFloatByResource =
      ["floating-center"]
    centerFloatLargeByResource =
      ["floating-center-large"]
    centerFloatByClass =
      ["Xfce4-settings-manager", "Pinentry"]
    centerFloatLargeByClass =
      ["Xfce4-appfinder"]

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
doublepad =  wrap " " "" . trim


myXmobarLogHook h = dynamicLogWithPP myXmobarPP

myXmobarPP = defaultPP
  { ppCurrent = xmobarColor myFocusedColor "" . wrap "-" "-"
  , ppVisible = xmobarColor myNormalColor "" . wrap " " " "
  , ppHidden  = const ""
  , ppUrgent  = xmobarColor myUrgentColor "" . wrap " !*" "*! "
  , ppTitle   = xmobarColor Sol.yellow "" .  trim
  , ppLayout  = const ""
  , ppSep     = xmobarColor Sol.yellow "" " :: "
  , ppSort    = getSortByXineramaRule
  }

myDzenLogHook h = dynamicLogWithPP $ myPP h

myPropLogHook = dynamicLogString myXmobarPP >>= xmonadPropLog

myPP h = defaultPP
  { ppCurrent = dzenColor Sol.base03 Sol.blue . doublepad
  , ppVisible = dzenColor Sol.blue "" . doublepad
  , ppHidden  = const ""
  , ppUrgent  = dzenColor Sol.base03 myUrgentColor . doublepad
  , ppTitle   = dzenColor Sol.yellow "" . dzenEscape . trim
  , ppLayout  = dzenColor Sol.base01 "" . trim
  , ppSep     = dzenColor Sol.cyan "" "  *  "
  , ppSort    = getSortByXineramaRule
  , ppOutput  = hPutStrLn h
  }
  --where
  --  padWs ws = if ws == "NSP" then "" else pad ws

------------------------------------------------------------------------
-- Startup hook
--
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--

myStartupHook = return ()


------------------------------------------------------------------------
-- Urgency hook

myUrgencyHook =
  withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = myUrgentColor }
    urgencyConfig
      { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--


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

-- myAutocompleteXPConfig = myXPConfig
--   { autoComplete = Just 500000  }

------------------------------------------------------------------------
-- Commands:

spawnShell :: X ()
spawnShell = spawn myTerminal

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
-- Default configuration

aDefaultConfig =
  addDescrKeys' ((confModMask, xK_F1), showKeybindings) myKeys $ defaultConfig
  { terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = 4
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalColor
  , focusedBorderColor = myFocusedColor
  , keys               = emptyKeys
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , startupHook        = myStartupHook
  } where
    --  | An empty keymap
    emptyKeys c = mkKeymap c [ ]

    -- | Display keyboard mappings using zenity
    showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
    showKeybindings x = addName "Show Keybindings" $ io $ do
      h <- spawnPipe "zenity --text-info"
      System.IO.UTF8.hPutStr h (unlines $ showKm x)
      hClose h
      return ()

-----------------------------------------------------------------------------
-- | Auto config!
--
autoConfig=do
  host <- fmap nodeName getSystemID
  return =<< chooseConfigByHost host
    where
      chooseConfigByHost c
        | c == "wonky"      = configSimple
        | otherwise        = configFull

-----------------------------------------------------------------------------
--
--  ConfigSimple is a default configuration with a simple xmobar setup
--
--  Should run and be compatible with most situations and quick set ups
--
--

configSimple = do
  return $ myUrgencyHook $ ewmh aDefaultConfig
    { logHook = myPropLogHook >> workspaceHistoryHook
    , manageHook = myManageHook
    , startupHook = myStartupHook
    }

-----------------------------------------------------------------------------
--
--  ConfigFull is an more involved setup with more tray bars and such
--
--
configFull = do
  return $ myUrgencyHook $ ewmh aDefaultConfig
    { logHook = myPropLogHook >> workspaceHistoryHook
    , manageHook = myManageHook
    , startupHook = myStartupHook
    }

-- | Show active workspace name slow
showWorkspaceName = showWorkspaceName1 2.5 Sol.yellow
-- | Show inactve workspace name slow
showWorkspaceNameOld = showWorkspaceName1 2.5 Sol.base1
-- | Show active workspace name fast
showWorkspaceNameFast = showWorkspaceName1 0.8 Sol.magenta
-- | Show inactive workspace name fast
showWorkspaceNameOldFast = showWorkspaceName1 0.8 Sol.base1

-- | Show workspace name
showWorkspaceName1 timeout bg = do
  ws <- gets (W.currentTag . windowset)
  DZ.dzenConfig
    (DZ.timeout timeout
     >=> DZ.onCurr (DZ.center 400 48)
     >=> DZ.font hugeFont
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
     >=> DZ.font hugeFont
     >=> DZ.addArgs ["-fg", Sol.base03]
     >=> DZ.addArgs ["-bg", Sol.green]
    ) ld

showInfo = do
  ws <- gets (W.currentTag . windowset)
  winset <- gets windowset
  wt <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let ld = description . W.layout . W.workspace . W.current $ winset
  DZ.dzenConfig
    (DZ.timeout 2
     >=> DZ.onCurr (DZ.center 700 48)
     >=> DZ.font (sizedFont "18")
     >=> DZ.addArgs ["-fg", Sol.base03]
     >=> DZ.addArgs ["-bg", Sol.orange]
     >=> DZ.addArgs ["-l", "1"]
     >=> DZ.addArgs ["-e", "onstart=uncollapse"]
     >=> DZ.addArgs ["-sa", "center"]
    ) (wt ++ "\n -" ++ ws ++ "-  ::  " ++ ld)

-- fill-column: 180
-- End:
