import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Instances ()
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Traversable(traverse)
import           Graphics.X11.Xinerama
import           System.IO
import qualified System.IO.UTF8
import           System.Environment (getArgs)

import qualified Solarized as Sol

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.UpdatePointer
import           XMonad.Config.Desktop
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ICCCMFocus
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.Replace (replace)
import qualified XMonad.StackSet as W

confModMask = mod4Mask

myTerminal = "urxvtc"

myConfig hs = let config = ewmh kde4Config { 
      modMask            = confModMask
    , handleEventHook    = handleEventHook kde4Config <+> fullscreenEventHook
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    , logHook            = myLogHook hs
    , startupHook        = setWMName "LG3D"
    , terminal           = myTerminal
    , normalBorderColor  = Sol.base02
    , focusedBorderColor = Sol.magenta
    } in addDescrKeys ((confModMask, xK_F1), showKeybindings) myKeys $ config
    where
        showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
        showKeybindings x = addName "Show Keybindings" $ io $ do
            h <- spawnPipe "zenity --text-info "
            System.IO.UTF8.hPutStr h (unlines $ showKm x)
            hClose h
            return ()

myLogHook hs = do
    multiPP'
        (mergePPOutputs [dynamicLogString . onlyTitle])
        myPP
        myPP { ppTitle = const "" }
        hs
    --updatePointer (Relative 0.95 0.95)
 
myManageHook = 
        manageHook kde4Config 
    <+> manageDocks 
    <+> namedScratchpadManageHook myScratchPads
    <+> (composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
    ])
  where myFloats      = ["MPlayer", "Yakuake", "Plasma", "Plasma-desktop"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
        ircApps       = ["Ksirc"]                -- open on desktop 3

myKeys conf = 
    subtitle "Cyclic display actions": mkNamedKeymap conf
    [ ("M-f",   addName "Next screen"                        $ nextScreen >> movePointer)
    , ("M-d",   addName "Previous screen"                    $ prevScreen >> movePointer)
    , ("M-C-f", addName "Swap current display witn next"     $ swapNextScreen >> nextScreen >> prevScreen >> movePointer )
    , ("M-C-d", addName "Swap current display witn previous" $ swapPrevScreen >> prevScreen >> nextScreen >> movePointer )
    , ("M-S-f", addName "Move window to next screen"         $ shiftNextScreen >> nextScreen >> movePointer )
    , ("M-S-d", addName "Move window to previous screen"     $ shiftPrevScreen >> prevScreen >> movePointer )
    ] ++
    subtitle "Application launching": mkNamedKeymap conf
    [ ("M-o v", toggleScratch "pamixer")
    , ("M-o h", toggleScratch "htop")
    , ("M-z", toggleScratch "terminal")
    , ("M-n", addName "Start new Pomodoro session" $ spawn "touch ~/.pomodoro_session")
    ]
    where
        movePointer = updatePointer (Relative 0.95 0.95)
        toggleScratch cmd' = addName("Toggle " ++ cmd' ++ " scratchpad ") $ namedScratchpadAction myScratchPads cmd'
        

myLayout = desktopLayoutModifiers $ avoidStruts $ smartBorders $ Full ||| tiled
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

myScratchPads = [ NS "terminal" (term "terminal") (res =? scratch "terminal") $ myCenterFloat 0.95 0.8
                --, termScratch "pamixer" $ myCenterFloat 0.7 0.2
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

getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> [Handle] -- ^ Handles for the status bars, in order of increasing X
                    -- screen number
        -> X ()
multiPP = multiPP' dynamicLogString
 
multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
    state <- get
    let pickPP :: WorkspaceId -> WriterT (Last XState) X String
        pickPP ws = do
            let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset state
            put state{ windowset = W.view ws $ windowset state }
            out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
            when isFoc $ get >>= tell . Last . Just
            return out
    traverse put . getLast
        =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
        =<< mapM screenWorkspace (zipWith const [0..] handles)
    return ()
 
mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp
 
onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { 
                           ppCurrent = const ""
                         , ppHidden = const ""
                         , ppVisible = const ""
                         , ppLayout = ppLayout pp
                         , ppTitle = ppTitle pp }
 
xmobarScreen :: Int -> IO Handle
xmobarScreen = spawnPipe . ("xmobar -x " ++) . show
 
myPP :: PP
myPP = sjanssenPP {
    ppLayout = xmobarColor "orange" "",
    ppUrgent = xmobarColor "red" "" . ('^':)
}

main :: IO ()
main = do
    args <- getArgs
    when ("--replace" `elem` args) replace
    xmonad . myConfig
        =<< mapM xmobarScreen =<< getScreens
