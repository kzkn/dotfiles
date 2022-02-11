import System.IO (hPutStrLn)

import XMonad
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad

-- myTerminal = "urxvtc"
myTerminal = "alacritty"
myWorkspaces = map show [1..5]

myBorderWidth = 3

myBlack = "#282828"
myRed = "#fb4934"
myYellow = "#b8bb26"
myGreen = "#fabd2f"
myBlue = "#83a598"
myMagenta = "#d3869b"
myCyan = "#83c07c"
myWhite = "#ebdbb2"
myGray = "#a89984"

gapWidth = 4
gwU = (U, 0)
gwD = (D, 0)
gwL = (L, 10)
gwR = (R, 10)

spawnXmobar = spawnPipe "xmobar $HOME/.xmonad/xmobarrc"

myLayout = toggleLayouts (noBorders Full) $ myLayoutTile

myLayoutTile = spacing gapWidth $ gaps [gwU, gwD, gwL, gwR] $ tiled ||| Mirror tiled
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppOutput = hPutStrLn h
  , ppOrder = \(ws:l:t:_) -> [ws, t]
  , ppCurrent = xmobarColor myMagenta myBlack . (\ws -> "●")
  , ppHidden = xmobarColor myWhite myBlack . (\ws -> "●")
  , ppHiddenNoWindows = xmobarColor myGray myBlack . (\ws -> "○")
  , ppTitle = xmobarColor myCyan myBlack . shorten 50
  }

myScratchpads = [
    NS "firefox" "firefox" (className =? "Firefox") defaultFloating
  , NS "chrome" "google-chrome-stable" (className =? "Google-chrome") defaultFloating
  ]

myBaseConfig = (ewmh . docks) def

main = do
  xmobar <- spawnXmobar
  xmonad $ myBaseConfig
    { focusedBorderColor = myRed
    , terminal = myTerminal
    , modMask = mod4Mask
    , borderWidth = myBorderWidth
    , workspaces = myWorkspaces
    , focusFollowsMouse = True
    , layoutHook = avoidStruts $ myLayout
    , logHook = myLogHook xmobar
    , handleEventHook = fullscreenEventHook <+> handleEventHook myBaseConfig
    }
    `additionalKeysP`
    [
      ("M-f" , sendMessage ToggleLayout)
    , ("M-p" , spawn "zsh -lc 'rofi -show run'")
    , ("S-M-p" , spawn "rofi -show ssh")
    , ("S-M-f" , namedScratchpadAction myScratchpads "firefox")
    , ("S-M-g" , namedScratchpadAction myScratchpads "chrome")
    , ("S-C-<Space>" , spawn "dunstctl close-all")
    , ("C-`" , spawn "dunstctl history-pop")
    ]
