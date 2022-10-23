import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import Graphics.X11.ExtraTypes.XF86
import System.Exit
  ( ExitCode (ExitSuccess),
    exitWith,
  )
import XMonad
import XMonad.Actions.PhysicalScreens
  ( sendToScreen,
    viewScreen,
  )
import qualified XMonad.Hooks.DynamicLog as D
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    fullscreenEventHook,
  )
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (..),
    avoidStruts,
    docks,
  )
import XMonad.Hooks.ManageHelpers
  ( Side (..),
    doCenterFloat,
    doSideFloat,
    isDialog,
  )
import XMonad.Hooks.UrgencyHook
  ( NoUrgencyHook (..),
    withUrgencyHook,
  )
import XMonad.Layout (Full (..), Tall (..))
import XMonad.Layout.NoBorders
  ( Ambiguity (..),
    lessBorders,
  )
import XMonad.Layout.Spacing
  ( Border (..),
    spacingRaw,
  )
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run
  ( hPutStrLn,
    runProcessWithInput,
    spawnPipe,
  )
import XMonad.Util.WorkspaceCompare (getSortByIndex)

promptConf =
  def
    { position = Bottom,
      font = "xft:Source Sans Pro:semibold:size=11",
      height = 44,
      bgColor = dark,
      fgColor = foreground,
      bgHLight = dark,
      fgHLight = highlight,
      promptBorderWidth = myPromptBorderWidth,
      borderColor = myPromptBorderColor,
      maxComplRows = Just 1,
      showCompletionOnTab = False
    }

-- | Float and resize window to the center of the screen.
centerWindow :: Window -> X ()
centerWindow win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  let newH = 11 / 12
  let newW = 3 / 5
  windows $
    W.float win (W.RationalRect ((1 - newW) / 2) ((1 - newH) / 2) newW newH)
  return ()

myWorkspaceKeys conf@(XConfig {XMonad.modMask = modMask}) =
  Map.fromList $
    [ ((modMask, xK_Return), spawn myTerminal),
      ((modMask, xK_p), shellPrompt promptConf),
      ((modMask, xK_b), spawn myBrowser),
      ((modMask, xK_v), spawn myEditor),
      ((modMask, xK_n), spawn "dunstctl history-pop"),
      ((modMask .|. shiftMask, xK_n), spawn "dunstctl close"),
      ((modMask, xK_q), spawn "lock"),
      ((modMask .|. shiftMask, xK_q), spawn "lock suspend"),
      ((modMask, xK_x), spawn "if type xmonad; then xmonad --restart; fi"),
      ((modMask .|. shiftMask, xK_x), io (exitWith ExitSuccess)),
      ((modMask .|. shiftMask, xK_c), kill),
      -- Layouts
      ((modMask, xK_space), sendMessage NextLayout),
      ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Move window focus
      ((modMask, xK_j), windows W.focusDown),
      ((modMask, xK_k), windows W.focusUp),
      ((modMask, xK_m), windows W.focusMaster),
      -- Modify the window order
      ((modMask .|. shiftMask, xK_j), windows W.swapDown),
      ((modMask .|. shiftMask, xK_k), windows W.swapUp),
      -- Resize windows
      ((modMask, xK_h), sendMessage Shrink),
      ((modMask, xK_l), sendMessage Expand),
      -- Floating layer
      ((modMask, xK_t), withFocused $ windows . W.sink),
      ((modMask, xK_f), withFocused $ centerWindow),
      -- Increase/decrease windos in master area
      ((modMask, xK_comma), sendMessage (IncMasterN 1)),
      ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    ]
      ++
      -- Switch screens based on physical position (xrandr).
      [ ((m .|. modMask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e] [0 ..],
          (f, m) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
      ]
      ++
      -- Switch workspaces.
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      -- Various function keys.
      [ ((0, xF86XK_MonBrightnessUp), spawn "bri laptop up"),
        ((0, xF86XK_MonBrightnessDown), spawn "bri laptop down"),
        ((0, xF86XK_AudioRaiseVolume), spawn "vol up"),
        ((0, xF86XK_AudioLowerVolume), spawn "vol down"),
        ((0, xF86XK_AudioMute), spawn "vol mute")
      ]

stringifyLayout :: String -> String
stringifyLayout l
  | t "Tall" l = fmt "tall"
  | t "Full" l = fmt "full"
  | otherwise = fmt l
  where
    t = List.isInfixOf
    fmt s = "[" ++ s ++ "]"

myLogHook h = do
  let fmt fg bg = D.pad . D.xmobarColor fg bg
  D.dynamicLogWithPP
    D.xmobarPP
      { D.ppCurrent = fmt primary "",
        D.ppHidden = fmt muted "",
        D.ppVisible = fmt foreground "",
        D.ppUrgent = fmt urgent "",
        D.ppLayout = fmt muted "" . stringifyLayout,
        D.ppTitle = const "",
        D.ppSep = " ",
        D.ppSort = getSortByIndex,
        D.ppOutput = hPutStrLn h
      }

myLayoutHook =
  uniformSpacing $
    lessBorders (OnlyScreenFloat) (tall ||| Full)
  where
    tall = Tall 1 (5 / 100) (1 / 2)
    gs = 5
    uniformSpacing = spacingRaw False (border) True (border) True
    border = Border gs gs gs gs

myManageHook =
  composeAll
    [ isDialog =? True --> doCenterFloat,
      className =? "Xmessage" --> doCenterFloat,
      className =? "Pavucontrol" --> doCenterFloat,
      fmap (List.isPrefixOf "zoom") className --> doCenterFloat
    ]

myConfig pipe =
  withUrgencyHook NoUrgencyHook $
    ewmh $
      docks
        def
          { logHook = myLogHook pipe,
            manageHook = myManageHook,
            layoutHook = avoidStruts $ myLayoutHook,
            handleEventHook = fullscreenEventHook,
            focusFollowsMouse = False,
            workspaces = myWorkspaces,
            terminal = myTerminal,
            modMask = myModMask,
            keys = myWorkspaceKeys,
            borderWidth = myBorderWidth,
            normalBorderColor = myUnfocusedBorderColor,
            focusedBorderColor = myFocusedBorderColor
          }

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- programs
myEditor = "editor -c"

myTerminal = "terminal"

myBrowser = "firefox"

-- colors
dark = "#36373d"

highlight = "#fcfcfc"

muted = "#828282"

foreground = "#ababb4"

primary = "#e8e8e8"

urgent = "#fa7610"

myFocusedBorderColor = "#36373d"

myUnfocusedBorderColor = "#ababb4"

myPromptBorderColor = "#26272d"

-- config vars
myBorderWidth = 2

myPromptBorderWidth = 1

myModMask = mod4Mask

myWorkspaces = ["def", "web", "dev", "misc"]
