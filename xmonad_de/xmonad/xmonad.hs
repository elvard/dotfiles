{-# OPTIONS_GHC -W -fno-warn-missing-signatures -fwarn-unused-imports #-}
-----------------------------------------------------------------------------
---- |
---- Copyright   :  (c) Tomas Ehrlich 2014-2314
---- License     :  WTFPL
----
---- Maintainer  :  tomas.ehrlich@gmail.com
---- Stability   :  unstableLayoutScreens
---- Portability :  unportable
----
---- My Xmonad config.
----
---- Requires latest xmonad, xmonad-contrib and xmobar.
----
---- Thanks to:
----    Thomas Frössman -- https://github.com/thomasf/dotfiles-thomasf-xmonad/
----    adamvo -- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo%27s_xmonad.hs
----
-------------------------------------------------------------------------------
import           Control.Monad (when)
import           System.IO
import qualified System.IO.UTF8
import           System.Environment (getArgs)

import qualified Solarized as Sol

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.UpdatePointer
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Fullscreen (fullscreenManageHook)
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.Replace (replace)
import           XMonad.Util.WorkspaceCompare

confModMask = mod4Mask

myTerminal = "urxvtc"

myConfig = let config = ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig { 
      modMask            = confModMask
    , handleEventHook    = myEventHook
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    , logHook            = myLogHook
    , startupHook        = myStartupHook
    , terminal           = myTerminal
    , normalBorderColor  = Sol.base02
    , focusedBorderColor = Sol.yellow
    } in addDescrKeys ((confModMask, xK_F1), showKeybindings) myKeys $ config
    where
        showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
        showKeybindings x = addName "Show Keybindings" $ io $ do
            h <- spawnPipe "zenity --text-info "
            System.IO.UTF8.hPutStr h (unlines $ showKm x)
            hClose h
            return ()

myLogHook = do
    multiPP focusedScreenPP unfocusedScreenPP
    updatePointer (Relative 0.95 0.95)

myEventHook = 
        handleEventHook defaultConfig 
    <+> fullscreenEventHook
    <+> dynStatusBarEventHook myStatusBar myStatusBarCleanup
 
myManageHook = 
        fullscreenManageHook
    <+> namedScratchpadManageHook myScratchPads
    <+> (composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ isFullscreen --> doFullFloat
      , isDialog     --> doFloat
      ]
    ])
    <+> manageDocks 
  where myFloats      = ["MPlayer", "Yakuake", "Plasma", "Plasma-desktop"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2

myStartupHook =
        dynStatusBarStartup myStatusBar myStatusBarCleanup
     >> setWMName "LG3D"

myKeys conf = 
    subtitle "Cyclic display actions": mkNamedKeymap conf
    [ ("M-f",   addName "Next screen"                        $ nextScreen >> movePointer)
    , ("M-d",   addName "Previous screen"                    $ prevScreen >> movePointer)
    , ("M-C-f", addName "Swap current display witn next"     $ swapNextScreen >> nextScreen >> prevScreen >> movePointer )
    , ("M-C-d", addName "Swap current display witn previous" $ swapPrevScreen >> prevScreen >> nextScreen >> movePointer )
    , ("M-S-f", addName "Move window to next screen"         $ shiftNextScreen >> nextScreen >> movePointer )
    , ("M-S-d", addName "Move window to previous screen"     $ shiftPrevScreen >> prevScreen >> movePointer )
    ] ++
    subtitle "Other window actions": mkNamedKeymap conf
    [ ("M-<Return>", addName "Swap the focused window and the master window" $ windows W.swapMaster >> movePointer)
    , ("M-t",        addName "Push the window into tiling mode"              $ withFocused (windows . W.sink) >> movePointer)
    , ("M-C-c",      addName "kill"                                            kill)
    , ("M-u",        addName "Focus urgent winow"                            $ focusUrgent >> movePointer )
    , ("M-C-u",      addName "Clear all urgent window statuses"              $ clearUrgents >> focusUrgent)
    ] ++
    subtitle "Application launching": mkNamedKeymap conf
    [ ("M-p",   addName "App launcher" $ shellPrompt myXPConfig)
    , ("M-S-p", addName "Favorite apps" $ spawnSelected defaultGSConfig ["google-chrome","pycharm", "dolphin"])
    , ("M-o v", toggleScratch "pamixer")
    , ("M-o h", toggleScratch "htop")
    , ("M-z",   toggleScratch "terminal")
    , ("M-n",   addName "Start new Pomodoro session" $ spawn "touch ~/.pomodoro_session")
    ]
    where
        movePointer = updatePointer (Relative 0.95 0.95)
        toggleScratch cmd' = addName("Toggle " ++ cmd' ++ " scratchpad ") $ namedScratchpadAction myScratchPads cmd'
        

myLayout = desktopLayoutModifiers $ smartBorders $ avoidStruts $ Full ||| tiled
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

myScratchPads = [ NS "terminal" (term "terminal") (res =? scratch "terminal") $ myCenterFloat 0.95 0.8
                , termScratch "htop" $ myCenterFloat 0.95 0.9]
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

myXPConfig = defaultXPConfig 
    { position = Bottom
    , bgColor = Sol.base03
    , fgColor = Sol.base2
    , bgHLight = Sol.base03
    , fgHLight = Sol.yellow
    , borderColor = Sol.base03
    , promptBorderWidth = 8
    , font = "xft:Droid Sans Mono:size=9:antialias=True"
}

focusedScreenPP :: PP
focusedScreenPP = namedScratchpadFilterOutWorkspacePP $ defaultPP {
      ppLayout  = xmobarColor Sol.yellow ""
    , ppCurrent = xmobarColor Sol.blue "" . wrap "[" "]"
    , ppVisible = xmobarColor Sol.base3 "" . wrap "<" ">"
    , ppUrgent  = xmobarColor Sol.red ""
    , ppTitle   = xmobarColor Sol.green ""
    , ppSep     = " | "
    , ppSort    = getSortByXineramaRule
}

unfocusedScreenPP :: PP
unfocusedScreenPP =  focusedScreenPP { ppTitle = xmobarColor Sol.base01 "" }
 
myStatusBar :: ScreenId -> IO Handle
myStatusBar (S 0) = spawnPipe "xmobar -x 0 ~/.xmobar.master"
myStatusBar (S s) = spawnPipe $ "xmobar -x " ++ show s ++ " ~/.xmobar.slave"

myStatusBarCleanup :: IO ()
myStatusBarCleanup = return ()

main :: IO ()
main = do
    args <- getArgs
    when ("--replace" `elem` args) replace
    xmonad myConfig
