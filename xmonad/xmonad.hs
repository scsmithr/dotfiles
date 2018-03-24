import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Hidden

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy,kill1)
import XMonad.Actions.CycleWS
import XMonad.Prompt

import XMonad.Util.EZConfig

import Data.List

-- colors
bg = "#282c32"
fg = "#abb2bf"
border = "#c678dd"
layoutFg = "#c678dd"
layoutBg = bg
selFg = bg
selBg = "#61afef"
visFg = selBg
visBg = bg

dmenuCmd = "dmenu_run -fn \"Source Code Pro-13\"\
    \ -nb \"" ++ bg ++ "\"\
    \ -nf \"" ++ fg ++ "\"\ 
    \ -sb \"" ++ selBg ++ "\"\
    \ -sf \"" ++ selFg ++ "\"\
    \ -p \">\""

layoutIcon :: String -> String
layoutIcon l
    | t "Tall" l = "|="
    | t "Full" l = "[]"
    | t "ThreeCol" l = "|||"
    | otherwise = l
    where t = isInfixOf

myPP = xmobarPP {
    ppCurrent = xmobarColor selFg selBg . pad,
    ppHidden = xmobarColor fg bg . pad,
    ppVisible = xmobarColor visFg visBg . pad,
    ppLayout = xmobarColor layoutFg layoutBg . layoutIcon,
    ppTitle = (\s -> ""),
    ppSep = " "
}

myLayoutHook = hiddenWindows $ smartBorders (tiled ||| Full ||| threeCol)
    where
        tiled = Tall nmaster delta ratio
        threeCol = ThreeColMid nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100
        

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

xConf = XPC { font          = "xft:Source Code Pro-12"
        , bgColor           = bg
        , fgColor           = fg
        , fgHLight          = selFg
        , bgHLight          = selBg
        , borderColor       = bg
        , promptBorderWidth = 1
        , promptKeymap      = defaultXPKeymap
        , completionKey     = (0,xK_Tab)
        , changeModeKey     = xK_grave
        , position          = Top
        , height            = 30
        , maxComplRows      = Nothing
        , historySize       = 256
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Nothing
        , showCompletionOnTab = False
        , searchPredicate   = isInfixOf
        , alwaysHighlight   = False
        }

conf = defaultConfig {
        logHook = dynamicLogWithPP myPP,
        layoutHook = myLayoutHook,

        focusFollowsMouse = False,
        terminal = "urxvt",
        modMask = mod4Mask,
        borderWidth = 2,
        normalBorderColor = bg,
        focusedBorderColor = border
    } 
    `additionalKeysP` 
    [
        -- lock/suspend
        ("M-S-l", spawn $ "i3lock -c \"" ++ bg ++ "\""),
        ("M-S-s", spawn $ "i3lock -c \"" ++ bg ++ "\"& sleep 2; systemctl suspend"),

        -- application shortcuts
        ("M-p", spawn dmenuCmd),
        ("M-b", spawn "firefox"),

        -- workspace management
        ("M-o", selectWorkspace xConf),
        ("M-r", renameWorkspace xConf),
        ("M-m", withWorkspace xConf (windows . W.shift)),
        ("M-S-m", withWorkspace xConf (windows . copy)),
        ("M-S-c", kill1), -- Rebind close so it only closes the copy
        ("M-S-<Backspace>", removeEmptyWorkspace),
        ("M-]", moveTo Next NonEmptyWS),
        ("M-[", moveTo Prev NonEmptyWS)   
    ]

main = do 
    xmonad =<< statusBar "xmobar" myPP toggleStruts conf 
