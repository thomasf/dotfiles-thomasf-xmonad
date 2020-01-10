{-# LANGUAGE FlexibleContexts,
  FlexibleInstances, MultiParamTypeClasses,
  NoMonomorphismRestriction, ScopedTypeVariables,
  TypeSynonymInstances, UndecidableInstances,
  PostfixOperators #-}
{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports -freduction-depth=99 #-}
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
import           Data.Ratio ((%))
import           Data.Ord
import           Data.Maybe
import qualified Solarized as Sol
import           System.Directory
import           System.Environment (getEnv)
import           System.Exit ( exitSuccess )
import           System.IO
import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleRecentWSAddons
import           XMonad.Actions.CycleWS hiding (toggleWS)
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.DynamicWorkspaceGroups
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseGestures
import qualified XMonad.Actions.Navigation2D as Nav2d
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer as WB
import           XMonad.Actions.WithAll
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(..), docksStartupHook)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook hiding (args)
import           XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import           XMonad.Layout.BoringWindows hiding (Replace)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.TwoPane
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.LayoutScreens as LS
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OnHost
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Layout.SimpleFloat (simpleFloat)
import qualified XMonad.Layout.Spiral as Spiral
import qualified XMonad.Prompt as XP
import           XMonad.Prompt.Workspace
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
import           XMonad.Util.Ungrab (unGrab)
import qualified Control.Arrow as Arrow
import qualified Graphics.Rendering.Pango.Layout as PL


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
  , ("M-o <Space>",addName "Goto workspace by window search prompt"        $ gotoWindow >> movePointer)
  , ("M-o s", spawnh "sshmenu")
  , ("M-o m", spawnh "moshmenu")
  , ("M-o c", spawnhm "google-chrome")
  , ("M-o f", spawnhm "firefox")
  , ("M-o u", spawnh' "x.uptime")
  , ("M-o i", spawnh' "x.info")
  , ("M-o j", spawnh' "x.todo")
  , ("M-o w", spawnhm "www")
  , ("M-o d", spawnhm "www-dev")
  , ("M-o t", spawnh "term")
  , ("M-o e", spawnhm "e")
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
  subtitle "Screen actions": mkNamedKeymap conf
  [ ("M-f",   addName "Next screen"                        $ showWorkspaceNameOld >> nextScreen >> movePointer >> showWorkspaceNameFast)
  , ("M-d",   addName "Previous screen"                    $ showWorkspaceNameOld >> prevScreen >> movePointer >> showWorkspaceNameFast)
  , ("M-C-f", addName "Swap current display witn next"     $ swapNextScreen >> nextScreen >> showWorkspaceNameOld >> prevScreen >> showWorkspaceName >> movePointer )
  , ("M-C-d", addName "Swap current display witn previous" $ swapPrevScreen >> prevScreen >> showWorkspaceNameOld >> nextScreen >> showWorkspaceName >> movePointer )
  , ("M-S-f", addName "Move window to next screen"         $ shiftNextScreen >> nextScreen >> movePointer )
  , ("M-S-d", addName "Move window to previous screen"     $ shiftPrevScreen >> prevScreen >> movePointer )
  ] ++
  subtitle "2D Navigation": mkNamedKeymap conf
  [ ("M-<Up>",      addName "Focus window above" $ Nav2d.windowGo U False)
  , ("M-<Down>",    addName "Focus window below" $ Nav2d.windowGo D False)
  , ("M-<Left>",    addName "Focus window left"  $ Nav2d.windowGo L False)
  , ("M-<Right>",   addName "Focus window right" $ Nav2d.windowGo R False)
  , ("M-C-<Up>",    addName "Swap screen above"  $ Nav2d.screenSwap U False >> showWorkspaceNameOld >> Nav2d.screenGo U False >> showWorkspaceNameFast)
  , ("M-C-<Down>",  addName "Swap screen below"  $ Nav2d.screenSwap D False >> showWorkspaceNameOld >> Nav2d.screenGo D False >> showWorkspaceNameFast)
  , ("M-C-<Left>",  addName "Swap screen left"   $ Nav2d.screenSwap L False >> showWorkspaceNameOld >> Nav2d.screenGo L False >> showWorkspaceNameFast)
  , ("M-C-<Right>", addName "Swap screen right"  $ Nav2d.screenSwap R False >> showWorkspaceNameOld >> Nav2d.screenGo R False >> showWorkspaceNameFast)
  , ("M-S-<Up>",    addName "Swap window above"  $ Nav2d.windowSwap U False)
  , ("M-S-<Down>",  addName "Swap window below"  $ Nav2d.windowSwap D False)
  , ("M-S-<Left>",  addName "Swap window left"   $ Nav2d.windowSwap L False)
  , ("M-S-<Right>", addName "Swap window right"  $ Nav2d.windowSwap R False)
  ] ++
  subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]": mkNamedKeymap conf
  [ ("M-e",         addName "Next workspace (prefix)"     $ rmEmptyWs $ nextWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-r",         addName "Previous workspace (prefix)" $ rmEmptyWs $ prevWsPrefix >> movePointer >> showWorkspaceNameFast)
  , ("M-C-e",       addName "New workspace in prefix.sequence" $ newPrefixWS >> movePointer >> showWorkspaceNameFast)
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
  , ("s-<XF86AudioPrev>",       spawnh' "mpc prev")
  , ("s-<XF86AudioNext>",       spawnh' "mpc next")
  , ("<XF86AudioPrev>",         spawnh' "mpc seek -00:00:10")
  , ("<XF86AudioNext>",         spawnh' "mpc seek +00:00:10")
  , ("C-<XF86AudioPrev>",       spawnh' "mpc seek -3%")
  , ("C-<XF86AudioNext>",       spawnh' "mpc seek +3%")
  , ("<XF86AudioStop>",         spawnh' "mpc stop")
  , ("<XF86MonBrightnessDown>", spawnh' "custom-backlight-macbookpro_gmux -m")
  , ("<XF86MonBrightnessUp>",   spawnh' "custom-backlight-macbookpro_gmux -p")
  ] ++
  subtitle "screenshot": mkNamedKeymap conf
  [ ("M-<Print>",     addName "sshot selected to clipboard" $ unGrab >> spawn "sshot")
  , ("M-C-<Print>",   addName "shhot focused window"        $ unGrab >> spawn "sshot focused")
  , ("M-M1-<Print>",  addName "shhot full"                  $ unGrab >> spawn "sshot full")
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
  [ ("M-5 2",  addName "Split current screen by 2 panes 0.5"              $ LS.layoutSplitScreen 2 $ smartSpacing 2 (TwoPane 0 0.5))
  , ("M-5 1",  addName "Split current screen by 2 panes 0.7"              $ LS.layoutSplitScreen 2 $ smartSpacing 2 (TwoPane 0 0.33))
  , ("M-5 3",  addName "Split current screen by 3 even columns"           $ LS.layoutSplitScreen 3 $ smartSpacing 2 (Mirror (Tall 3 0 0)))
  -- , ("M-5 e",  addName "Split current screen by 3 larger center"          $ LS.layoutSplitScreen 3 $ smartSpacing 2 $ (ThreeColMid 1 (3/100) (4/10)))
  , ("M-5 4",  addName "Split current screen by 3 columns + bottom wide"  $ LS.layoutSplitScreen 4 $ smartSpacing 2 $ Mirror (Tall 3 0 (5/6)))
  , ("M-5 m",  addName "Merge screens 1 + 2"                              $ LS.layoutScreens 2  Full)
  , ("M-5 r",  addName "Reset screens from xinerama, rescreen"              rescreen)
  ] ++
  subtitle "Toggle scratchpads and workspaces": mkNamedKeymap conf
  [ ("M-<Space>",           toggleScratch "largeTerminal")
  , ("M-i a",               myViewWS' "android")
  , ("M-i b",               myViewWS' "vbox")
  , ("M-i c",               myViewWS' "chat")
  , ("M-i d",               myViewWS' "dash")
  , ("M-i f",               myViewWS' "files")
  , ("M-i h",               myViewWS' "home")
  , ("M-i m",               myViewWS' "mail")
  , ("M-i n",               myViewWS' "nodes")
  , ("M-i s",               myViewWS' "s")
  , ("M-i w",               myViewWS' "www")
  , ("M-i j",               myViewWS' "work")
  , ("M-i k",               myViewWS' "worklib")
  , ("M-i M-i",             addName "cycle ws"                              $ rmEmptyWs $ myCycleRecentWs xK_i xK_o)
  , ("M-i i",               addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePromptHidden >> maybeWorkspaceAction >> movePointer)
  -- , ("M-C-i M-C-i",         addName "cycle ws on next screen"               $ holdScreenFocus $ nextScreen >> myCycleRecentWs xK_i xK_o)
  -- , ("M-C-u M-C-u",         addName "cycle ws on prev screen"               $ holdScreenFocus $ prevScreen >> myCycleRecentWs xK_o xK_i)
  , ("M-i <Space> <Space>", addName "Create or change workspace prompt"     $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
  , ("M-S-i",               addName "Move window to other workspace prompt" $ DW.withWorkspace xpc (windows . W.shift) >> movePointer >> updateStruts)
  , ("M-i <Space> r",       addName "Rename current workspace"              $ DW.renameWorkspace xpc >> movePointer)
  , ("M-i <Backspace>",     addName "Delete current workspace"              $ DW.removeWorkspace >> movePointer)
  , ("M-p w",               addName ".www"                                  $ gotoPrefixWS "www" >> movePointer)
  , ("M-p d",               addName ".doc"                                  $ gotoPrefixWS "doc" >> movePointer)
  , ("M-p c",               addName ".code"                                 $ gotoPrefixWS "code" >> movePointer)
  , ("M-p r",               addName ".remote"                               $ gotoPrefixWS "remote" >> movePointer)
  , ("M-p s",               addName ".s"                                    $ gotoPrefixWS "s" >> movePointer)
  , ("M-p f",               addName ".files"                                $ gotoPrefixWS "files" >> movePointer)
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
 subtitle "exit/quit/leave/reboot...": mkNamedKeymap conf
 [ ("M-q r",             addName "restart xmonad"                       $ restart "xmonad" True)
 , ("M-q x x x",         addName "restart xmonad without keeping state" $ restart "xmonad" False)
 , ("M-q k k k",         addName "KILL xmonad"                          $ io exitSuccess)
 , ("M-q <Space>",       addName "xmenu"                                $ spawn "xmenu")
 ]
  where
    spawnh' cmd' = addName cmd'           $ spawn cmd'
    spawnh  cmd' = addName cmd' $ bindOn' $ spawn cmd'
    spawnhm cmd' = addName cmd' $ bindOn' $ showAppName cmd' >> spawn cmd'

    -- | Remove current workpace if empty
    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept [ "NSP", "", "s.0", "s.1", "s.2", "s.3", "s.4" ]


    -- | View a workspace by name, remove left over empty workspace and move pointer
    myViewWS' wsid = addName("Show " ++ wsid ++ " workspace ") $ do
      rmEmptyWs $ myViewWS wsid
      movePointer
    wsPrompt wsp = wsp xpc afterWSPrompt

    -- | Select workspae prompt
    selectWorkspacePrompt = wsPrompt workspacePrompt

    -- | Select workspace prompt
    selectWorkspacePromptHidden = wsPrompt workspacePromptHidden
    -- selectWorkspacePromptHidden = workspacePrompt'

    -- | Select workspace promt only showing non visible workspaces
    workspacePromptHidden :: XP.XPConfig -> (String -> X ()) -> X ()
    workspacePromptHidden c job = do ws <- gets (W.hidden . windowset)
                                     s <- getSortByIndex
                                     -- thisWS <- gets (W.currentTag . windowset)
                                     let ts = map W.tag $ s ws
                                         -- prefix = takeWhile (/= wsSeparator) thisWS
                                         prefix = ""
                                     XP.mkXPrompt (Wor "") c {XP.defaultText=prefix} (XP.mkComplFunFromList' ts) job


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
     where options w = map (W.view `flip` w) (recentTags w)
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

    -- | Returns the window name as will be listed in dmenu.
    --   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
    --   know where he's going.
    decorateName ws w = do
      wName <- show <$> getName w
      cName <- getClass w
      let wm = PL.escapeMarkup wName
          cn = PL.escapeMarkup cName
          tn = PL.escapeMarkup (W.tag ws)
      return $ printf "%s<span font_family='Consolas'> — %s</span> ❨%s❩" wm cn tn

    getClass w = do
      classHint <- withDisplay $ \d -> io $ getClassHint d w
      return $ resClass classHint

    gotoWindow = WB.gotoMenuConfig def {
      WB.menuCommand="rofi",
      WB.menuArgs=["-markup-rows", "-dmenu", "-i", "-no-custom"],
      windowTitler=decorateName }




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
                 [ ([    ], \_ -> gridselectWorkspace myGsconfig W.greedyView)
                 , ([R, D], \_ -> sendMessage NextLayout)
                 , ([U   ], \w -> focus w >> Nav2d.windowSwap U False)
                 , ([D   ], \w -> focus w >> Nav2d.windowSwap D False)
                 , ([L   ], \w -> focus w >> Nav2d.windowSwap L False)
                 , ([R   ], \w -> focus w >> Nav2d.windowSwap R False)
                 ]




-- | Colors
myFocusedColor = Sol.magenta
myFocusedColor2 darkmode = if darkmode then Sol.magenta else Sol.magentaL
myUrgentColor = Sol.blue
myNormalBorderColor darkmode = if darkmode then Sol.base02 else Sol.base2
myGsconfig = def
    { gs_cellheight = 70
    , gs_cellwidth = 150
    , gs_cellpadding = 6
    , gs_font = sizedXftFont "13"
    }

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
  onWorkspace "worklib" (fullBorder ||| tabs ||| gridWide) .
  onWorkspace "upgrade" alternative .
  onWorkspace "im" im $
  lessBorders OnlyScreenFloat
  standard
  where
    -- control
    first = wide
    standard = onTall
               (first ||| tcol ||| grid ||| gridWide ||| spiral ||| oneBig )
               (first ||| tall ||| tabs ||| gridWide ||| spiral ||| oneBig)
    alternative = onTall
               (tcol ||| first ||| grid)
               (first ||| tall ||| tabs ||| gridWide ||| spiral ||| oneBig)
    onTall = onHosts ["transwhale"]
    -- helpers
    refmin = mkToggle (single REFLECTX)
    mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True
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
    gridWide = rename "grid" . ss . refmin $ GridRatio (16/9)
    grid = rename "grid" . ss . refmin $ GridRatio (4/3)
    im = onTall (im' 2) (im' 1)
    im' r =  withIM (r%9) pidginRoster . reflectHoriz $ withIM (r%8) skypeRoster
         (gridWide ||| grid ||| spiral)
      where
        pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
        skypeRoster  = ClassName "Skype"
                       `And` Not (Title "Options")
                       `And` Not (Title "Add a Skype Contact")
                       `And` Not (Title "Start a conference call")
                       `And` Not (Title "Terms of Use")
                       `And` Not (Role "ConversationsWindow")
                       `And` Not (Role "CallWindowForm")
                       `And` Not (Role "CallWindow")




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
  , [className =? "Skype" <&&> title =? "Options" -?> doCenterFloatLarge]
  , [className =? "Skype" <&&> startsWith' title "Profile for " -?> doCenterFloat]
  , [className =? "Skype" <&&> startsWith' title "Add a Skype Contact" -?> doCenterFloatLarge]
  , [resource  =? r -?>                                        doFloat            | r <- ["floating"]]
  , [className =? c -?>                                        doFloat            | c <- ["Unity-2d-launcher", "Orage", "feh"]]
  -- NOTE chrome does note set title early enough for this
  -- , [role      =? "pop-up" <&&> appName =? "google-chrome" <&&> startsWith' title "Developer Tools - "  -?> doSink]
  , [role      =? "pop-up" <&&> appName =? "google-chrome" -?> doCenterFloatLarge]
  , [role      =? "GtkFileChooserDialog" -?> doCenterFloatLarge]
  , [role      =? "bubble" -?>                                 doIgnore]
  , [className =? "Zenity" <&&> title =? "Question" -?>        doCenterFloat]
  , [className =? "Zenity" -?>                                 doCenterFloatLarge]
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
    doCenterFloatLarge = myCenterFloat 0.95 0.85
    doSink = ask >>= doF . W.sink
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
myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> serverModeEventHook



-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myXmobarTopPP = def
  { ppCurrent = xmobarColor myFocusedColor "" . wrap " " "" . trim
  , ppVisible = wrap " " "" . trim
  , ppHidden  = const ""
  , ppUrgent  = wrap " ◀"  "▶"
  , ppTitle   = const ""
  , ppLayout  = const ""
  , ppSep     = " "
  , ppSort    = mySortByXineramaPhysicalRule
  }

myXmobarBottomPP = def
  { ppCurrent = const ""
  , ppVisible = const ""
  , ppHidden  = const ""
  , ppUrgent  = const ""
  , ppTitle   = trim
  , ppLayout  = const ""
  , ppSep     = xmobarColor Sol.cyan "" " + "
  , ppSort    = mySortByXineramaPhysicalRule
  }

myLogHook = do
  dynamicLogString myXmobarTopPP >>= xmonadPropLog' "_XMONAD_LOG_TOP"
  dynamicLogString myXmobarBottomPP >>= xmonadPropLog' "_XMONAD_LOG_BOTTOM"
  workspaceHistoryHook
  ewmhDesktopsLogHook
  -- fadeInactiveLogHook 0.86
  setWMName "LG3D"

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
    onScreen w =  W.current w : W.visible w
    isOnScreen a w  = a `elem` map (W.tag . W.workspace) (onScreen w)
    tagToSid s x = W.screen $ fromJust $ find ((== x) . W.tag . W.workspace) s
    cmpPosition False w a b = comparing (tagToSid $ onScreen w) a b
    cmpPosition True w a b = comparing (rect . tagToSid (onScreen w)) a b
      -- where rect i = let (Rectangle x y _ _) = screens !! fromIntegral i in (y,x)
      where rect i = let (Rectangle x y _ _) = screens !! fromIntegral i in (x,y)
            screens = map (screenRect . W.screenDetail) $ sortBy (comparing W.screen) $ W.current w : W.visible w



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

defXPConfig = def
 { XP.position = XP.CenteredAt 0.4 0.5
 , XP.promptBorderWidth = 1
 , XP.font = sizedXftFont "20"
 , XP.height = 24
 , XP.promptKeymap = emacsLikeXPKeymap' (\c -> isSpace c || c == '.')
 }
myXPConfig darkmode = if darkmode
  then defXPConfig
 { XP.bgColor = Sol.base02
 , XP.fgColor = Sol.base1
 , XP.bgHLight = Sol.base02
 , XP.fgHLight = Sol.magenta
 , XP.borderColor = Sol.base02
 } else defXPConfig
 { XP.bgColor = Sol.base2
 , XP.fgColor = Sol.base01
 , XP.bgHLight = Sol.base2
 , XP.fgHLight = Sol.magenta
 , XP.borderColor = Sol.base2
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
  , (xK_space, setSomething)
  ] where
    setSomething = do
      XP.setInput ""
      XP.endOfLine



-- Scratch pads:

myScratchPads = [ NS "largeTerminal" (term "largeTerminal") (res =? scratch "largeTerminal") $ myCenterFloat 0.95 0.8
                , termScratch "pamixer" $ myCenterFloat 0.7 0.2
                , termScratch "htop" $ myCenterFloat 0.95 0.9
                ]
  where
    scratch sname = "scratchpad_" ++ sname
    -- term sname = myTerminal ++ " -name " ++ scratch sname
    term sname = " urxvt " ++ " -name " ++ scratch sname
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
  home <- io $ getEnv "HOME"
  darkmode <- doesFileExist $ home ++ "/.config/darkmode"
  return . docks . Nav2d.withNavigation2DConfig myNavigation2DConfig . myUrgencyHook . addDescrKeys' ((confModMask, xK_F1), showKeybindings) (myKeys $ myXPConfig darkmode) $ def {
    terminal           = myTerminal
  , focusFollowsMouse  = False
  , borderWidth        = myDefaultBorderWidth
  , modMask            = confModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor darkmode
  , focusedBorderColor = myFocusedColor2 darkmode
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
    , ("worklib", altCmd)
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
  unkillable appn titl res = (appn == "ssh_tmux") || titl == "XMonad" || res == "floating-center-large"
  scratchTerm appn = appn == "scratchpad_largeTerminal"
  askToKill appn role = (appn == "google-chrome" && role == "browser") || (appn=="urxvt") || (appn=="Alacitty")
  killOrElse win appn titl res role
    | alwayskillable titl = killWindow win
    | unkillable appn titl res = return ()
    | scratchTerm appn = namedScratchpadAction myScratchPads "largeTerminal"
    | askToKill appn role  = bindOn' killPrompt
    | otherwise = bindOn' $ killWindow win

killPrompt :: X ()
killPrompt = withFocused $ \win ->
  runSelectedAction myGsconfig
        [ ("Kill active window", killWindow win)
        , ("Cancel", return ())
        ]

killPrompt' :: X ()
killPrompt' = withFocused $ \w ->
  runSelectedAction myGsconfig
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
showWorkspaceName = showWorkspaceName' showStatusMultipleMessagesDuration Sol.yellow
-- | Show inactve workspace name slow
showWorkspaceNameOld = showWorkspaceName' showStatusMultipleMessagesDuration Sol.base1
-- | Show active workspace name fast
showWorkspaceNameFast = showWorkspaceName' showStatusSingleMessageDuration Sol.magenta

showAppName = showMessage 1 Sol.cyan Sol.base03

getScreenRect :: X Rectangle
getScreenRect = (screenRect . W.screenDetail . W.current) <$> gets windowset

showMessage timeout bg fg msg = do
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
showWorkspaceName' timeout bg = do
  wsn <- gets (W.currentTag . windowset)
  showMessage timeout bg Sol.base03 wsn

-- | Show current layout name
showLayoutName = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset
  showMessage 2 Sol.green Sol.base03 ld


-- | Enforce recaluclation of docks gaps. After updating a ~5month old xmonad master today docks were not being avoided by avoudStruts on new desktop.
updateStruts = docksStartupHook

-- -- -- -- WIP dmenu everywhere... there is a grab problem...


-- dmc ws = do
--   DM.dmenu (map W.tag ws)

-- udmenu opts = dmenu opts

-- workspacePrompt' :: X ()
-- workspacePrompt' = do
--   ws <- gets (map W.tag . W.hidden . windowset)
--   -- ws <- gets (W.hidden . windowset)
--   unGrab
--   let choice = dmenu $ ws
--   do ch <- choice
--      afterWSPrompt  ch
--      -- afterWSPrompt $ DW.withWorkspaceIndex ch
--   -- do ch <- DW.withWorkspaceIndex choice
--      -- afterWSPrompt $ DW.withWorkspaceIndex ch
--          -- liftIO
--          -- let a=   DM.dmenu (map W.tag ws)
--          -- myViewWS $ DW.withWorkspaceIndex aa
--       -- return aa

-- choices alist = do
--   answer <- DM.dmenu $ map fst alist
--   when ((not . null) answer) (io $ maybe (return ()) id $ lookup answer alist)

-- -- | Run dmenu to select an option from a list.
-- dmenu :: [String] -> X String
-- dmenu opts = menu "dmenu" opts

-- -- | like 'dmenu' but also takes the command to run.
-- menu :: String -> [String] -> X String
-- menu menuCmd opts = menuArgs menuCmd [] opts

-- -- | Like 'menu' but also takes a list of command line arguments.
-- menuArgs :: String -> [String] -> [String] -> X String
-- menuArgs menuCmd args opts = fmap (filter (/='\n')) $ runProcessWithInput menuCmd args (unlines opts)



-- Local Variables:
-- fill-column: 165
-- shell-command-after-save-cmd: "xmonad --recompile && xmonad --restart"
-- End:
