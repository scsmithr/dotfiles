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
import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , isDialog
                                                )
import           XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..)
                                                , withUrgencyHook
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )

import           XMonad.Layout.NoBorders        ( Ambiguity(..)
                                                , lessBorders
                                                )
import           XMonad.Layout.ResizableTile    ( ResizableTall(..)
                                                , MirrorResize(..)
                                                )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )
import           XMonad.Layout.ThreeColumns     ( ThreeCol(..) )

import           XMonad.Actions.DynamicWorkspaces
                                                ( removeEmptyWorkspace )
import           XMonad.Actions.PhysicalScreens ( viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.CycleWS         ( toggleWS )

import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.WorkspaceCompare   ( getSortByIndex )
import           XMonad.Util.Run                ( spawnPipe
                                                , hPutStrLn
                                                , runProcessWithInput
                                                )

import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Map.Strict               as StrictMap

import qualified RofiPrompt

applicationKeys :: [(String, X ())]
applicationKeys =
  [ ("M-<Return>", spawn myTerminal)
  , ("M-b"       , spawn "firefox")
  , ("M-v"       , spawn "emacs")
  , ("M-q"       , spawn "lock")
  , ("M-S-s"     , spawn "lock suspend")
  ]

windowManagementKeys :: [(String, X ())]
windowManagementKeys =
  [ ("M-S-c"       , kill)
  , ("M-<Space>"   , sendMessage NextLayout)
  , ("M-t"         , withFocused $ windows . W.sink)
  , ("M-f"         , withFocused $ float)
  , ("M-n"         , sendMessage ToggleStruts)
  , ("M--"         , toggleWS)
  , ("M-j"         , windows W.focusDown)
  , ("M-k"         , windows W.focusUp)
  , ("M-S-j"       , windows W.swapDown)
  , ("M-S-k"       , windows W.swapUp)
  , ("M-S-<Return>", windows W.swapMaster)
  , ("M-h"         , sendMessage Shrink)
  , ("M-l"         , sendMessage Expand)
  , ("M-S-h"       , sendMessage MirrorExpand)
  , ("M-S-l"       , sendMessage MirrorShrink)
  , ("M-,"         , sendMessage (IncMasterN 1))
  , ("M-."         , sendMessage (IncMasterN (-1)))
  ]

mediaKeys :: [(String, X ())]
mediaKeys =
  [ ("<XF86MonBrightnessUp>"  , spawn "bri laptop up")
  , ("<XF86MonBrightnessDown>", spawn "bri laptop down")
  , ("<XF86AudioRaiseVolume>" , spawn "vol up")
  , ("<XF86AudioLowerVolume>" , spawn "vol down")
  , ("<XF86AudioMute>"        , spawn "vol mute")
  ]

myKeys = applicationKeys ++ windowManagementKeys ++ mediaKeys

stringifyLayout :: String -> String
stringifyLayout l | t "Tall" l     = "tall"
                  | t "Full" l     = "full"
                  | t "ThreeCol" l = "col"
                  | otherwise      = l
  where t = List.isInfixOf

myLogHook h = do
  let fmt fg bg = D.pad . D.xmobarColor fg bg
  D.dynamicLogWithPP D.xmobarPP { D.ppCurrent = fmt primary ""
                                , D.ppHidden  = fmt muted ""
                                , D.ppVisible = fmt foreground ""
                                , D.ppUrgent  = fmt urgent ""
                                , D.ppLayout  = fmt muted "" . stringifyLayout
                                , D.ppTitle   = const ""
                                , D.ppSep     = " "
                                , D.ppSort    = getSortByIndex
                                , D.ppOutput  = hPutStrLn h
                                }

myLayoutHook = uniformSpacing
  $ lessBorders (OnlyScreenFloat) (tiled ||| Full ||| threeCol)
 where
  tiled          = ResizableTall nmaster delta (1 / 2) []
  threeCol       = ThreeCol nmaster delta (1 / 3)
  nmaster        = 1
  delta          = 3 / 100
  gs             = 5
  uniformSpacing = spacingRaw False (border) True (border) True
  border         = Border gs gs gs gs

myManageHook = composeAll
  [ className =? "mpv" --> doCenterFloat
  , className =? "feh" --> doCenterFloat
  , isDialog =? True --> doCenterFloat
  ]

myConfig pipe = withUrgencyHook NoUrgencyHook $ ewmh $ docks $ additionalKeysP
  def { logHook            = myLogHook pipe
      , manageHook         = myManageHook
      , layoutHook         = avoidStruts $ myLayoutHook
      , handleEventHook    = fullscreenEventHook
      , focusFollowsMouse  = False
      , workspaces         = myWorkspaces
      , terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myUnfocusedBorderColor
      , focusedBorderColor = myFocusedBorderColor
      }
  myKeys

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- colors
muted = "#657b83"
foreground = "#a3b4b6"
primary = "#268bd2"
urgent = "#cb4b16"

-- config vars
myFocusedBorderColor = "#a3b4b6"
myUnfocusedBorderColor = "#c6d3d3"
myBorderWidth = 2
myTerminal = "terminal"
myModMask = mod4Mask
myWorkspaces = ["def", "email", "web", "dev"]
