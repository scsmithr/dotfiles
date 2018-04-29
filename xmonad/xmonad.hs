import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import XMonad.Actions.SwapWorkspaces

import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig

import Data.Char
import Data.List

-- colors
bg = "#282c32"

mutedBg = "#24282e"

fg = "#abb2bf"

border = "#c678dd"

promptBorder = "#98c379"

layoutFg = "#c678dd"

layoutBg = bg

selFg = "#61afef"

selBg = bg

visFg = "#98c379"

visBg = bg

urgentFg = "#e5c07b"

layoutIcon :: String -> String
layoutIcon l
    | t "Tall" l = fmt "|="
    | t "Full" l = fmt "[]"
    | t "ThreeCol" l = fmt "|||"
    | otherwise = l
  where
    t = isInfixOf
    fmt = pad

hideIfProjectWS :: WorkspaceId -> String
hideIfProjectWS ws
    | isNumber ws = ws
    | otherwise = ""
  where
    isNumber "" = False
    isNumber "." = False
    isNumber xs =
        case dropWhile isDigit xs of
            "" -> True
            ('.':ys) -> all isDigit ys
            _ -> False

myPP =
    xmobarPP
        { ppCurrent = xmobarColor selFg selBg . pad
        , ppHidden = xmobarColor fg bg . pad . hideIfProjectWS
        , ppVisible = xmobarColor visFg visBg . pad
        , ppUrgent = xmobarColor urgentFg bg . pad
        , ppLayout = xmobarColor layoutFg layoutBg . layoutIcon
        , ppTitle = const ""
        , ppSep = ""
        }

myLayoutHook = hiddenWindows $ smartBorders (tiled ||| Full ||| threeCol)
  where
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

xConf =
    XPC
        { font = "xft:Source Code Pro Medium-12"
        , bgColor = bg
        , fgColor = fg
        , fgHLight = selFg
        , bgHLight = selBg
        , borderColor = promptBorder
        , promptBorderWidth = 0
        , promptKeymap = defaultXPKeymap
        , completionKey = (0, xK_Tab)
        , changeModeKey = xK_grave
        , position = Top
        , height = 36
        , maxComplRows = Just 10
        , historySize = 256
        , historyFilter = id
        , defaultText = []
        , autoComplete = Nothing
        , showCompletionOnTab = False
        , searchPredicate = isInfixOf
        , alwaysHighlight = False
        }

data Launcher =
    Launcher

instance XPrompt Launcher where
    showXPrompt Launcher = "> "
    completionToCommand _ = escape

escape :: String -> String
escape [] = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar = flip elem " &\\@\"'#?$*()[]{};"

launcherPrompt :: XPConfig -> X ()
launcherPrompt c = do
    cmds <- io getCommands
    mkXPrompt Launcher c (getShellCompl cmds $ searchPredicate c) spawn

modm = mod4Mask

conf =
    withUrgencyHook NoUrgencyHook $
    defaultConfig
        { logHook = dynamicLogWithPP myPP
        , layoutHook = myLayoutHook
        , focusFollowsMouse = False
        , terminal = "urxvt"
        , modMask = modm
        , borderWidth = 2
        , normalBorderColor = mutedBg
        , focusedBorderColor = border
        } `additionalKeysP`
    -- lock/suspend
    [ ("M-S-l", spawn $ "i3lock -c \"" ++ bg ++ "\"")
    , ( "M-S-s"
      , spawn $ "i3lock -c \"" ++ bg ++ "\"& sleep 2; systemctl suspend")
    -- application shortcuts
    , ("M-p", launcherPrompt xConf) -- replaces dmenu
    , ("M-b", spawn "firefox")
    -- workspace management
    , ("M-o", selectWorkspace xConf)
    , ("M-S-o", renameWorkspace xConf)
    , ("M-m", withWorkspace xConf (windows . W.shift))
    , ("M-S-m", withWorkspace xConf (windows . copy))
    , ("M-S-c", kill1) -- Rebind close so it only closes the copy
    , ("M-S-<Backspace>", removeEmptyWorkspace)
    , ("M-]", moveTo Next NonEmptyWS)
    , ("M-[", moveTo Prev NonEmptyWS)
    ] `additionalKeys`
    zip (zip (repeat modm) [xK_1 .. xK_9])
        (map (withNthWorkspace W.greedyView) [0 ..]) `additionalKeys`
    zip (zip (repeat (modm .|. shiftMask)) [xK_1 .. xK_9])
        (map (withNthWorkspace W.shift) [0 ..])

main = xmonad =<< statusBar "xmobar" myPP toggleStruts conf
