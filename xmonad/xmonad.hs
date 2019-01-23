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
import qualified Hideable

myWorkspaces = ["def", "conn", "email", "web"]

myKeys =
  [ ("M-S-l"     , spawn $ "lock")
  , ("M-S-s"     , spawn $ "lock suspend")
  , ("M-S-c"     , kill)
  , ("M-q"       , spawn "xmonad --restart")
  , ("M-<Space>" , sendMessage NextLayout)
  , ("M-S-q"     , io (exitWith ExitSuccess))
  , ("M-t"       , withFocused $ windows . W.sink)
  , ("M-n"       , sendMessage ToggleStruts)
  , ("M-<Return>", spawn myTerminal)
  , ("M-p"       , spawn "rofi -show run")
  , ("M-b"       , spawn "firefox")
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
         , ( (0, xK_s)
           , do
             writeStateToFile
             notify "xmonad" "wrote state file"
           )
         ]
      ++ zip (zip (repeat (0)) [xK_1 .. xK_9])
             (map (PinnedWorkspaces.pinCurrentWorkspace) [1 ..])
    )
  , ("M--"                    , toggleWS)
  , ("M-j"                    , windows W.focusDown)
  , ("M-k"                    , windows W.focusUp)
  , ("M-S-j"                  , windows W.swapDown)
  , ("M-S-k"                  , windows W.swapUp)
  , ("M-S-<Return>"           , windows W.swapMaster)
  , ("M-f"                    , withFocused Hideable.pushHidden)
  , ("M-g"                    , Hideable.popHidden)
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

notify :: String -> String -> X ()
notify title msg = do
  spawn $ "notify-send '" ++ title ++ "' '" ++ msg ++ "'"

formatIcon
  :: String -- icon bg
  -> String -- icon fg
  -> String -- layout name
  -> String
formatIcon bg fg l | t "Tall" l     = fmt "|="
                   | t "Full" l     = fmt "[]"
                   | t "ThreeCol" l = fmt "|||"
                   | otherwise      = l
 where
  t   = List.isInfixOf
  fmt = D.pad . (D.xmobarColor fg bg)

formatWorkspace
  :: (WorkspaceId -> Maybe PinnedWorkspaces.PinnedIndex)
  -> (WorkspaceId -> Maybe Int)
  -> String -- index bg
  -> String -- index fg
  -> String -- workspace bg
  -> String -- workspace fg
  -> Bool -- show workspace
  -> WorkspaceId -- workspace name
  -> String
formatWorkspace getIndex getNumHidden idxBg idxFg wsBg wsFg mustShow ws =
  case idx of
    Just n  -> (D.xmobarColor idxFg idxBg $ (show n) ++ ":") ++ wsStr
    Nothing -> case mustShow of
      True  -> wsStr
      False -> ""
 where
  idx       = getIndex ws
  hiddenStr = case getNumHidden ws of
    Just n  -> D.xmobarColor idxFg idxBg $ "[" ++ show n ++ "]"
    Nothing -> ""
  wsStr = (D.xmobarColor wsFg wsBg ws) ++ hiddenStr

myLogHook h = do
  wmap <- PinnedWorkspaces.getMap
  let getIndex ws = PinnedWorkspaces.getIndex (StrictMap.toList wmap) ws

  hmap <- Hideable.getMap
  let getNumHidden ws = StrictMap.lookup ws hmap

  let fmt = formatWorkspace getIndex getNumHidden
  D.dynamicLogWithPP D.xmobarPP { D.ppCurrent = fmt "" muted "" selFg True
                                , D.ppHidden  = fmt "" muted "" hiddenFg False
                                , D.ppVisible = fmt "" muted "" visFg True
                                , D.ppUrgent  = fmt "" muted "" urgentFg True
                                , D.ppLayout  = formatIcon "" layoutFg
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

myLayoutHook = Hideable.hiddenWindows
  $ smartBorders (tiled ||| Full ||| threeCol)
 where
  tiled          = uniformSpacing $ Tall nmaster delta ratio
  threeCol       = uniformSpacing $ ThreeCol nmaster delta ratio
  nmaster        = 1
  ratio          = 1 / 2
  delta          = 3 / 100
  gs             = 2
  uniformSpacing = spacingRaw False (border) True (border) True
  border         = Border gs gs gs gs

myStartupHook = return ()

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
      , normalBorderColor  = unfocused
      , focusedBorderColor = focused
      , startupHook        = myStartupHook
      }
  myKeys

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- foreground/ background colors
bg = "#282c34"
muted = "#495162"
fg = "#abb2bf"
selFg = "#61afef"
visFg = "#98c379"
hiddenFg = fg
layoutFg = "#c678dd"
urgentFg = "#e5c07b"

-- borders
focused = "#61afef"
unfocused = "#394152"
myBorderWidth = 2

myTerminal = "launch_alacritty"
myModMask = mod4Mask
