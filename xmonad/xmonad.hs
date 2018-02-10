import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns


import Data.List

-- colors
bg = "#282c32"
fg = "#abb2bf"
border = "#c678dd"
layoutFg = "#c678dd"
layoutBg = bg
selFg = bg
selBg = "#61afef"

layoutIcon :: String -> String
layoutIcon l
    | isInfixOf "Tall" l = "|="
    | isInfixOf "Full" l = "[]"
    | isInfixOf "Three" l = "|||"
    | otherwise = l

myPP = xmobarPP {
    ppCurrent = xmobarColor selFg selBg . pad,
    ppHidden = xmobarColor fg bg . pad,
    ppVisible = xmobarColor fg bg . pad,
    ppLayout = layoutIcon . xmobarColor layoutFg layoutBg,
    ppTitle = (\s -> ""),
    ppSep = " "
}

myLayoutHook = smartBorders (tiled ||| Full ||| multiTile)
    where
        tiled = Tall nmaster delta ratio
        multiTile = multiCol [1,1,1,1,1,0] 1 delta (-0.5)
        nmaster = 1
        ratio = 1/2
        delta = 3/100
        

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

main = do
    xmonad =<< statusBar "xmobar" myPP toggleStruts defaultConfig {
        logHook = dynamicLogWithPP myPP,
        layoutHook = myLayoutHook,

        focusFollowsMouse = False,
        terminal = "urxvt",
        modMask = mod4Mask,
        borderWidth = 2,
        normalBorderColor = bg,
        focusedBorderColor = border
    }
