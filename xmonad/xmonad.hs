import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

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

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run ( spawnPipe, hPutStrLn )
import XMonad.Util.WorkspaceCompare

import Data.Char
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Map as M ( fromList )
import qualified Data.Map.Strict as SM

layoutIcon :: String -> String
layoutIcon l
    | t "Tall" l = fmt "|="
    | t "Full" l = fmt "[]"
    | t "ThreeCol" l = fmt "|||"
    | otherwise = l
  where
    t = isInfixOf
    fmt = pad

myWorkspaces = ["def", "conn", "email", "web"]

myKeys = [
    -- Lock/sleep
      ("M-S-l", spawn $ "i3lock -c \"" ++ bg ++ "\"")
    , ( "M-S-s", spawn $ "i3lock -c \"" ++ bg ++ "\"& sleep 2; systemctl suspend")
    -- general shortcuts
    , ("M-S-c", kill)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-q", spawn "xmonad --restart")
    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-n", sendMessage ToggleStruts)
    -- application shortcuts
    , ("M-<Return>", spawn myTerminal)
    , ("M-p", launcherPrompt xConf) -- replaces dmenu
    , ("M-b", spawn "firefox")
    -- workspace management
    , ("M-o", selectWorkspace xConf)
    , ("M-S-o", withWorkspace xConf (windows . W.shift))
    , ("M-<Backspace>", removeWsPin)
    , ("M-S-<Backspace>", removeEmptyWorkspace)
    , ("M-u", toggleWS)
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
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5 -fps 60")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5 -fps 60")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute @DEFAULT_SINK@ false; pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-mute @DEFAULT_SINK@ false; pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    ]

type WorkspaceTag = String
type PinnedIndex = Int

data PinnedWorkspaceState = PinnedWorkspaceState {
        pinnedWorkspaceMap :: SM.Map PinnedIndex WorkspaceTag
    }
    deriving (Typeable, Read, Show)

instance ExtensionClass PinnedWorkspaceState where
    initialValue = PinnedWorkspaceState SM.empty
    extensionType = PersistentExtension

setPinnedIndex :: PinnedIndex -> X()
setPinnedIndex idx = do
    wtag <- gets (W.currentTag . windowset)
    wmap <- XS.gets pinnedWorkspaceMap
    let cleared = clearIfPinned wmap wtag
    XS.modify $ \s -> s {pinnedWorkspaceMap = SM.insert idx wtag cleared}
    -- TODO: Shifting to this workspace is just used to force xmonad to 
    -- trigger an event so that our log hook is called. Figure out how to 
    -- avoid needing to shift.
    withPinnedIndex W.shift idx

removeWsPin :: X()
removeWsPin = do
    wtag <- gets (W.currentTag . windowset)
    wmap <- XS.gets pinnedWorkspaceMap
    XS.modify $ \s -> s {pinnedWorkspaceMap = clearIfPinned wmap wtag}
    windows $ W.greedyView wtag -- Same "workaround" as above

clearIfPinned :: (SM.Map PinnedIndex WorkspaceTag) -> WorkspaceId -> (SM.Map PinnedIndex WorkspaceTag)
clearIfPinned wmap w = case pinnedLookup (SM.toList wmap) w of
    Just idx -> SM.delete idx wmap
    Nothing -> wmap

withPinnedIndex :: (String -> WindowSet -> WindowSet) -> PinnedIndex -> X()
withPinnedIndex job pidx = do
    wtag <- ilookup pidx
    maybe (return ()) (windows . job) wtag
        where
            ilookup :: PinnedIndex -> X (Maybe WorkspaceTag)
            ilookup idx = SM.lookup idx `fmap` XS.gets pinnedWorkspaceMap

myWorkspaceKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
    ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..],
        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withPinnedIndex W.greedyView) [1..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withPinnedIndex W.shift) [1..])
    ++
    zip (zip (repeat (modm .|. controlMask)) [xK_1..xK_9]) (map (setPinnedIndex) [1..])

pinnedCompare :: Maybe Int -> Maybe Int -> Ordering
pinnedCompare Nothing Nothing = EQ
pinnedCompare Nothing (Just _) = GT
pinnedCompare (Just _) Nothing = LT
pinnedCompare a b = compare a b

pinnedLookup :: [(PinnedIndex, WorkspaceId)] -> WorkspaceId -> Maybe PinnedIndex
pinnedLookup l w = 
    case find (\(_,x) -> x == w) l of
        Just p -> Just(fst p) 
        Nothing -> Nothing

getPinnedIdx :: X (WorkspaceId -> Maybe Int)
getPinnedIdx = do
    wmap <- XS.gets pinnedWorkspaceMap
    return (pinnedLookup $ SM.toList wmap)

getPinnedCompare :: X WorkspaceCompare
getPinnedCompare = do
    idx <- getPinnedIdx
    return $ mconcat [pinnedCompare `on` idx, compare]

getSortByPinned :: X WorkspaceSort
getSortByPinned = mkWsSort getPinnedCompare

indexPref :: PinnedIndex -> String -> String
indexPref idx ws = (show idx) ++ ":" ++ ws

hideIfNotPinned :: Maybe PinnedIndex -> String -> String
hideIfNotPinned idx ws = case idx of
    Just n -> indexPref n ws
    Nothing -> ""

showWorkspace :: Maybe PinnedIndex -> String -> String
showWorkspace idx ws = case idx of
    Just n -> indexPref n ws
    Nothing -> ws

myLogHook h = do
    wmap <- XS.gets pinnedWorkspaceMap
    let getWorkspaceName fn color ws = xmobarColor color "" (fn (pinnedLookup (SM.toList wmap) ws) ws)
   
    dynamicLogWithPP xmobarPP
        { ppCurrent = getWorkspaceName showWorkspace selFg
        , ppHidden = getWorkspaceName hideIfNotPinned fg
        , ppVisible = getWorkspaceName showWorkspace visFg
        , ppUrgent = getWorkspaceName showWorkspace urgentFg
        , ppLayout = xmobarColor layoutFg layoutBg . layoutIcon
        , ppTitle = const ""
        , ppSep = ""
        , ppSort = getSortByPinned
        , ppOutput = hPutStrLn h
        }

myLayoutHook = smartBorders (tiled ||| Full ||| threeCol)
  where
    tiled = smartSpacing gs $ Tall nmaster delta ratio
    threeCol = smartSpacing gs $ ThreeCol nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    gs = 5

toggleStruts XConfig {modMask = modMask} = (modMask, xK_n)

xConf =
    XPC
        { font = "xft:Source Code Pro Medium-11"
        , bgColor = darkBg
        , fgColor = fg
        , fgHLight = selFg
        , bgHLight = darkBg
        , borderColor = promptBorder
        , promptBorderWidth = myBorderWidth
        , promptKeymap = defaultXPKeymap
        , completionKey = (0, xK_Tab)
        , changeModeKey = xK_grave
        , position = CenteredAt 0.05 0.4
        , height = 44
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
promptBorder = darkBg 

-- config vars
myTerminal = "kitty"
myModMask = mod4Mask
myBorderWidth = 2
