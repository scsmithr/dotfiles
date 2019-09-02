import           XMonad

import qualified XMonad.StackSet               as W
import           XMonad.Operations              ( writeStateToFile )

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

import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.ResizableTile    ( ResizableTall(..)
                                                , MirrorResize(..)
                                                )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )

import           XMonad.Actions.DynamicWorkspaces
                                                ( removeEmptyWorkspace )
import           XMonad.Actions.PhysicalScreens ( viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.Submap          ( submap )
import           XMonad.Actions.CycleWS         ( toggleWS )

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

applicationKeys :: [(String, X ())]
applicationKeys =
  [ ("M-<Return>", spawn myTerminal)
  , ("M-p"       , spawn "rofi -show run")
  , ("M-b"       , spawn "firefox")
  ]

windowManagementKeys :: [(String, X ())]
windowManagementKeys =
  [ ("M-q"      , spawn "lock")
  , ("M-S-s"    , spawn "lock suspend")
  , ("M-S-c"    , kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-t"      , withFocused $ windows . W.sink)
  , ("M-n"      , sendMessage ToggleStruts)
  , ("M-o"      , RofiPrompt.selectWorkspace)
  , ("M-S-o"    , RofiPrompt.moveToWorkspace)
  , ("M-S-y"    , RofiPrompt.renameWorkspace)
  , ( "M-u"
    , submap
      .  Map.fromList
      $  [ ((0, xK_u), PinnedWorkspaces.unpinCurrentWorkspace)
         , ((0, xK_d), removeEmptyWorkspace)
         ]
      ++ zip (zip (repeat (0)) [xK_1 .. xK_9])
             (map (PinnedWorkspaces.pinCurrentWorkspace) [1 ..])
    )
  , ( "M-x"
    , RofiPrompt.exec
      "xmonad"
      [ ("state"    , writeStateToFile)
      , ("restart"  , spawn "xmonad --restart")
      , ("recompile", spawn "xmonad --recompile && xmonad --restart")
      , ("quit"     , io (exitWith ExitSuccess))
      ]
      (\s -> notify "xmonad" $ "unknown option: " ++ s)
    )
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

notify :: String -> String -> X ()
notify title msg = do
  spawn $ "notify-send '" ++ title ++ "' '" ++ msg ++ "'"

formatIcon
  :: String -- icon bg
  -> String -- icon fg
  -> String -- layout name
  -> String
formatIcon bg fg l | t "Tall" l && t "Mirror" l = fmt "=="
                   | t "Tall" l                 = fmt "|="
                   | t "Full" l                 = fmt "[]"
                   | otherwise                  = fmt l
 where
  t   = List.isInfixOf
  fmt = D.pad . (D.xmobarColor fg bg)

formatWorkspace
  :: (WorkspaceId -> Maybe PinnedWorkspaces.PinnedIndex)
  -> String -- index bg
  -> String -- index fg
  -> String -- workspace bg
  -> String -- workspace fg
  -> Bool -- show workspace
  -> WorkspaceId -- workspace name
  -> String
formatWorkspace getIndex idxBg idxFg wsBg wsFg mustShow ws = case idx of
  Just n  -> (D.xmobarColor idxFg idxBg $ (show n) ++ ":") ++ wsStr
  Nothing -> case mustShow of
    True  -> wsStr
    False -> ""
 where
  idx   = getIndex ws
  wsStr = (D.xmobarColor wsFg wsBg ws)

myLogHook h = do
  wmap <- PinnedWorkspaces.getMap
  let getIndex ws = PinnedWorkspaces.getIndex (StrictMap.toList wmap) ws

  let fmt = formatWorkspace getIndex
  D.dynamicLogWithPP D.xmobarPP { D.ppCurrent = fmt "" muted "" primary True
                                , D.ppHidden  = fmt "" muted "" muted False
                                , D.ppVisible = fmt "" muted "" foreground True
                                , D.ppUrgent  = fmt "" muted "" urgent True
                                , D.ppLayout  = formatIcon "" muted
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
    ++ [((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)]

myLayoutHook = uniformSpacing (tiled ||| Full)
 where
  tiled          = ResizableTall nmaster delta (1 / 2) []
  nmaster        = 1
  delta          = 3 / 100
  gs             = 4
  uniformSpacing = spacingRaw False (border) True (border) True
  border         = Border gs gs gs gs

myStartupHook :: X ()
myStartupHook = do
  let indexes = zip myWorkspaces [1 ..]
  mapM_ (\x -> PinnedWorkspaces.pinWorkspace (fst x) (snd x)) indexes

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
      , keys               = myWorkspaceKeys
      , terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myUnfocusedBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , startupHook        = myStartupHook
      }
  myKeys

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- colors
muted = "#96A7A9"
foreground = "#556b72"
primary = "#268bd2"
urgent = "#cb4b16"

-- config vars
myFocusedBorderColor = "#c6c6c6"
myUnfocusedBorderColor = "#F2E6CE"
myBorderWidth = 1
myTerminal = "terminal"
myModMask = mod4Mask
myWorkspaces = ["def", "email", "web", "dev"]
