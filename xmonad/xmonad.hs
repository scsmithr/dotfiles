import XMonad
import XMonad.StackSet

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns
import XMonad.Layout.Hidden

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
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

dmenuCmd = "dmenu_run -fn \"Source Code Pro-15\"\
    \ -nb \"" ++ bg ++ "\"\
    \ -nf \"" ++ fg ++ "\"\ 
    \ -sb \"" ++ selBg ++ "\"\
    \ -sf \"" ++ selFg ++ "\"\
    \ -p \">\""

layoutIcon :: String -> String
layoutIcon l
    | isInfixOf "Tall" l = "|="
    | isInfixOf "Full" l = "[]"
    | isInfixOf "MultiCol" l = "|||"
    | otherwise = l

myPP = xmobarPP {
    ppCurrent = xmobarColor selFg selBg . pad,
    ppHidden = xmobarColor fg bg . pad,
    ppVisible = xmobarColor visFg visBg . pad,
    ppLayout = xmobarColor layoutFg layoutBg . layoutIcon,
    ppTitle = (\s -> ""),
    ppSep = " "
}

myLayoutHook = hiddenWindows $ smartBorders (tiled ||| Full ||| multiTile)
    where
        tiled = Tall nmaster delta ratio
        multiTile = multiCol [1,1,0] 1 delta (-0.5)
        nmaster = 1
        ratio = 1/2
        delta = 3/100
        

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

xConf = XPC { font          = "xft:Source Code Pro-15"
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
        , searchPredicate   = isPrefixOf
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
    } `additionalKeysP` 
    [
        ("M-p", spawn dmenuCmd),
        ("M-S-l", spawn $ "i3lock -c \"" ++ bg ++ "\""),
        ("M-S-s", spawn $ "i3lock -c \"" ++ bg ++ "\"& sleep 2; systemctl suspend"),
        ("M-b", spawn "firefox"),
        ("M-o", selectWorkspace xConf),
        ("M-S-o", renameWorkspace xConf),
        ("M-m", withWorkspace xConf (windows . shift)),
        ("M-S-d", removeEmptyWorkspace),
        ("M-]", moveTo Next NonEmptyWS),
        ("M-[", moveTo Prev NonEmptyWS)
    ]

main = do 
    xmonad =<< statusBar "xmobar" myPP toggleStruts conf 
