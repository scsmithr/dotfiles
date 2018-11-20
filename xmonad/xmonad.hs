import XMonad
import qualified XMonad.StackSet as W

import System.Exit ( exitWith, ExitCode ( ExitSuccess ) )

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run ( spawnPipe, hPutStrLn, runProcessWithInput )

import Data.Char
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Map as M ( fromList )

import qualified RofiPrompt
import qualified PinnedWorkspaces

myWorkspaces = ["def", "conn", "email", "web"]

myKeys = [
    -- Lock/sleep
      ("M-S-l", spawn $ "i3lock -c \"" ++ darkBg ++ "\"")
    , ( "M-S-s", spawn $ "i3lock -c \"" ++ darkBg ++ "\"& sleep 2; systemctl suspend")
    -- general shortcuts
    , ("M-S-c", kill)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-q", spawn "xmonad --restart")
    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-n", sendMessage ToggleStruts)
    -- application shortcuts
    , ("M-<Return>", spawn myTerminal)
    , ("M-p", spawn "rofi -show run")
    , ("M-b", spawn "chromium")
    , ("M-i", submap . M.fromList $
        [ ((0, xK_f), spawn "nautilus")
        , ((0, xK_p), spawn "screenshot.sh")
        ])
    -- workspace management
    , ("M-o", RofiPrompt.selectWorkspace)
    , ("M-S-o", RofiPrompt.withWorkspace (windows . W.shift))
    , ("M-u", submap . M.fromList $
        [ ((0, xK_u), PinnedWorkspaces.unpinCurrentWorkspace)
        , ((0, xK_d), removeEmptyWorkspace)
        ] ++ zip (zip (repeat (0)) [xK_1..xK_9]) (map (PinnedWorkspaces.pinCurrentWorkspace) [1..]))
    -- window management
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    -- function keys
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2 -fps 60")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2 -fps 60")
    , ("<XF86AudioRaiseVolume>", spawn "vol up")
    , ("<XF86AudioLowerVolume>", spawn "vol down")
    , ("<XF86AudioMute>", spawn "vol mute")
    ]

layoutIcon :: String -> String
layoutIcon l
    | t "Tall" l = fmt "|="
    | t "Full" l = fmt "[]"
    | t "ThreeCol" l = fmt "|||"
    | otherwise = l
  where
    t = isInfixOf
    fmt = pad

indexPref :: PinnedWorkspaces.PinnedIndex -> String -> String
indexPref idx ws = (show idx) ++ ":" ++ ws

hideIfNotPinned :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
hideIfNotPinned idx ws = case idx of
    Just n -> indexPref n ws
    Nothing -> ""

showWorkspace :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
showWorkspace idx ws = case idx of
    Just n -> indexPref n ws
    Nothing -> ws

showCurrentWorkspace :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
showCurrentWorkspace idx ws = showWorkspace idx ws

myLogHook h = do
    getIndex <- PinnedWorkspaces.indexReader
    let format fn fg bg ws = xmobarColor fg bg (fn (getIndex ws) ws)

    dynamicLogWithPP xmobarPP
        { ppCurrent = format showCurrentWorkspace selFg ""
        , ppHidden = format hideIfNotPinned fg ""
        , ppVisible = format showWorkspace visFg ""
        , ppUrgent = format showWorkspace urgentFg ""
        , ppLayout = xmobarColor layoutFg "" . layoutIcon
        , ppTitle = const ""
        , ppSep = ""
        , ppSort = PinnedWorkspaces.getSortByPinned
        , ppOutput = hPutStrLn h
        }

myWorkspaceKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
    ((m .|. modm, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..],
        (f, m) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
    ]
    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (PinnedWorkspaces.withPinnedIndex W.greedyView) [1..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (PinnedWorkspaces.withPinnedIndex W.shift) [1..])

myLayoutHook = smartBorders (tiled ||| Full ||| threeCol)
  where
    tiled = smartSpacing gs $ Tall nmaster delta ratio
    threeCol = smartSpacing gs $ ThreeCol nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    gs = 5

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

myStartupHook = return ()

myManageHook = composeAll []

myConfig pipe = withUrgencyHook NoUrgencyHook $ ewmh $ docks $ def
    { logHook = myLogHook pipe
    , manageHook = myManageHook
    , layoutHook = avoidStruts $ myLayoutHook
    , focusFollowsMouse = False
    , workspaces = myWorkspaces
    , keys = myWorkspaceKeys
    , terminal = myTerminal
    , modMask = myModMask
    , borderWidth = myBorderWidth
    , normalBorderColor = mutedBg
    , focusedBorderColor = border
    , startupHook = myStartupHook
    } `additionalKeysP` myKeys

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- colors
bg = "#282c34"
mutedBg = "#353b45"
darkBg = "#21252E"
selBg = bg
visBg = bg
layoutBg = bg

fg = "#abb2bf"
selFg = "#61afef"
visFg = "#98c379"
layoutFg = "#c678dd"
urgentFg = "#e5c07b"

border = "#61afef"
promptBorder = mutedBg 

-- config vars
myTerminal = "urxvt"
myModMask = mod4Mask
myBorderWidth = 2
