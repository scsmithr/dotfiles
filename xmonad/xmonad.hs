import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig

import Data.List

-- colors
bg = "#282c32"

mutedBg = "#24282e"

fg = "#abb2bf"

border = "#c678dd"

promptBorder = "#98c379"

layoutFg = "#c678dd"

layoutBg = bg

selFg = bg

selBg = "#61afef"

visFg = selBg

visBg = bg

urgentFg = "#e5c07b"

layoutIcon :: String -> String
layoutIcon l
    | t "Tall" l = "|="
    | t "Full" l = "[]"
    | t "ThreeCol" l = "|||"
    | otherwise = l
  where
    t = isInfixOf

myPP =
    xmobarPP
        { ppCurrent = xmobarColor selFg selBg . pad
        , ppHidden = xmobarColor fg bg . pad
        , ppVisible = xmobarColor visFg visBg . pad
        , ppUrgent = xmobarColor urgentFg bg . pad
        , ppLayout = xmobarColor layoutFg layoutBg . layoutIcon
        , ppTitle = const ""
        , ppSep = " "
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

conf =
    withUrgencyHook NoUrgencyHook $
    defaultConfig
        { logHook = dynamicLogWithPP myPP
        , layoutHook = myLayoutHook
        , focusFollowsMouse = False
        , terminal = "urxvt"
        , modMask = mod4Mask
        , borderWidth = 2
        , normalBorderColor = mutedBg
        , focusedBorderColor = border
        } `additionalKeysP`
    -- lock/suspend
    [ ("M-S-l", spawn $ "i3lock -c \"" ++ bg ++ "\"")
    , ( "M-S-s"
      , spawn $ "i3lock -c \"" ++ bg ++ "\"& sleep 2; systemctl suspend")
    -- application shortcuts
    , ("M-p", launcherPrompt xConf)
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
    -- Urgent stuff
    , ("M-u", focusUrgent)
    ]

main = xmonad =<< statusBar "xmobar" myPP toggleStruts conf
