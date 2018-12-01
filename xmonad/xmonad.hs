import           XMonad
import qualified XMonad.StackSet               as W

import           System.Exit                    ( exitWith
                                                , ExitCode(ExitSuccess)
                                                )

import qualified XMonad.Hooks.DynamicLog       as D
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..)
                                                , docks
                                                , avoidStruts
                                                )
import           XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..)
                                                , withUrgencyHook
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )

import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.ThreeColumns     ( ThreeCol(..) )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )

import           XMonad.Actions.DynamicWorkspaces
                                                ( removeEmptyWorkspace )
import           XMonad.Actions.PhysicalScreens ( viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.Submap          ( submap )

import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.Run                ( spawnPipe
                                                , hPutStrLn
                                                , runProcessWithInput
                                                )

import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Map.Strict               as StrictMap

import qualified RofiPrompt
import qualified PinnedWorkspaces

myWorkspaces = ["def", "conn", "email", "web"]

myKeys =
  [ ("M-S-l", spawn $ "i3lock -c \"" ++ darkBg ++ "\"")
  , ( "M-S-s"
    , spawn $ "i3lock -c \"" ++ darkBg ++ "\"& sleep 2; systemctl suspend"
    )
  , ("M-S-c"     , kill)
  , ("M-q"       , spawn "xmonad --restart")
  , ("M-<Space>" , sendMessage NextLayout)
  , ("M-S-q"     , io (exitWith ExitSuccess))
  , ("M-t"       , withFocused $ windows . W.sink)
  , ("M-n"       , sendMessage ToggleStruts)
  , ("M-<Return>", spawn myTerminal)
  , ("M-p"       , spawn "rofi -show run")
  , ("M-b"       , spawn "chromium")
  , ( "M-i"
    , submap
      . Map.fromList
      $ [((0, xK_f), spawn "nautilus"), ((0, xK_p), spawn "screenshot.sh")]
    )
  , ("M-o"  , RofiPrompt.selectWorkspace)
  , ("M-S-o", RofiPrompt.withWorkspace (windows . W.shift))
  , ( "M-u"
    , submap
      .  Map.fromList
      $  [ ((0, xK_u), PinnedWorkspaces.unpinCurrentWorkspace)
         , ((0, xK_d), removeEmptyWorkspace)
         ]
      ++ zip (zip (repeat (0)) [xK_1 .. xK_9])
             (map (PinnedWorkspaces.pinCurrentWorkspace) [1 ..])
    )
  , ("M-j"                    , windows W.focusDown)
  , ("M-k"                    , windows W.focusUp)
  , ("M-S-j"                  , windows W.swapDown)
  , ("M-S-k"                  , windows W.swapUp)
  , ("M-S-<Return>"           , windows W.swapMaster)
  , ("M-h"                    , sendMessage Shrink)
  , ("M-l"                    , sendMessage Expand)
  , ("M-,"                    , sendMessage (IncMasterN 1))
  , ("M-."                    , sendMessage (IncMasterN (-1)))
  , ("<XF86MonBrightnessUp>"  , spawn "bri laptop up")
  , ("<XF86MonBrightnessDown>", spawn "bri laptop down")
  , ("<XF86AudioRaiseVolume>" , spawn "vol up")
  , ("<XF86AudioLowerVolume>" , spawn "vol down")
  , ("<XF86AudioMute>"        , spawn "vol mute")
  ]


layoutIcon :: String -> String
layoutIcon l | t "Tall" l     = fmt "|="
             | t "Full" l     = fmt "[]"
             | t "ThreeCol" l = fmt "|||"
             | otherwise      = l
 where
  t   = List.isInfixOf
  fmt = D.pad

indexPref :: PinnedWorkspaces.PinnedIndex -> String -> String
indexPref idx ws = (show idx) ++ ":" ++ ws

hideIfNotPinned :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
hideIfNotPinned idx ws = case idx of
  Just n  -> indexPref n ws
  Nothing -> ""

showWorkspace :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
showWorkspace idx ws = case idx of
  Just n  -> indexPref n ws
  Nothing -> ws

showCurrentWorkspace :: Maybe PinnedWorkspaces.PinnedIndex -> String -> String
showCurrentWorkspace idx ws = showWorkspace idx ws

myLogHook h = do
  wmap <- PinnedWorkspaces.getMap
  let format fn fg bg ws = D.xmobarColor fg bg
        $ fn (PinnedWorkspaces.getIndex (StrictMap.toList wmap) ws) ws
  D.dynamicLogWithPP D.xmobarPP
    { D.ppCurrent = format showCurrentWorkspace selFg ""
    , D.ppHidden  = format hideIfNotPinned fg ""
    , D.ppVisible = format showWorkspace visFg ""
    , D.ppUrgent  = format showWorkspace urgentFg ""
    , D.ppLayout  = D.xmobarColor layoutFg "" . layoutIcon
    , D.ppTitle   = const ""
    , D.ppSep     = ""
    , D.ppSort    = PinnedWorkspaces.getSortByPinned
    , D.ppOutput  = hPutStrLn h
    }

myWorkspaceKeys conf@(XConfig { XMonad.modMask = modm }) =
  Map.fromList
    $  [ ((m .|. modm, key), f sc)
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f  , m ) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
       ]
    ++ zip (zip (repeat (modm)) [xK_1 .. xK_9])
           (map (PinnedWorkspaces.withPinnedIndex W.greedyView) [1 ..])
    ++ zip (zip (repeat (modm .|. shiftMask)) [xK_1 .. xK_9])
           (map (PinnedWorkspaces.withPinnedIndex W.shift) [1 ..])


myLayoutHook = smartBorders (tiled ||| Full ||| threeCol)
 where
  tiled          = uniformSpacing $ Tall nmaster delta ratio
  threeCol       = uniformSpacing $ ThreeCol nmaster delta ratio
  nmaster        = 1
  ratio          = 1 / 2
  delta          = 3 / 100
  gs             = 4
  uniformSpacing = spacingRaw False (border) True (border) True
  border         = Border gs gs gs gs

toggleStruts XConfig { modMask = modMask } = (modMask, xK_n)

myStartupHook = return ()

myManageHook = composeAll []

myConfig pipe = withUrgencyHook NoUrgencyHook $ ewmh $ docks $ additionalKeysP
  def { logHook            = myLogHook pipe
      , manageHook         = myManageHook
      , layoutHook         = avoidStruts $ myLayoutHook
      , handleEventHook    = fullscreenEventHook
      , focusFollowsMouse  = False
      , workspaces         = myWorkspaces
      , keys               = myWorkspaceKeys
      , terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = mutedBg
      , focusedBorderColor = border
      , startupHook        = myStartupHook
      }
  myKeys

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
