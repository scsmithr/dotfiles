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
                                                , doSideFloat
                                                , Side(..)
                                                )
import           XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..)
                                                , withUrgencyHook
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Layout.BinarySpacePartition
                                                ( emptyBSP
                                                , Rotate(..)
                                                , FocusParent(..)
                                                , TreeBalance(..)
                                                , ResizeDirectional(..)
                                                )
import           XMonad.Actions.Navigation2D    ( windowSwap
                                                , windowGo
                                                , switchLayer
                                                , Navigation2DConfig(..)
                                                , sideNavigation
                                                , withNavigation2DConfig
                                                , Direction2D(..)
                                                )
import           XMonad.Layout.NoBorders        ( Ambiguity(..)
                                                , lessBorders
                                                )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )
import           XMonad.Actions.PhysicalScreens ( viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.CycleWS         ( toggleWS )
import           XMonad.Util.WorkspaceCompare   ( getSortByIndex )
import           XMonad.Util.Run                ( spawnPipe
                                                , hPutStrLn
                                                , runProcessWithInput
                                                )
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           Graphics.X11.ExtraTypes.XF86
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Map.Strict               as StrictMap

promptConf = def { position            = Bottom
                 , font                = "xft:Fira Sans Medium-10"
                 , height              = 44
                 , bgColor             = dark
                 , fgColor             = foreground
                 , bgHLight            = dark
                 , fgHLight            = light
                 , promptBorderWidth   = myPromptBorderWidth
                 , borderColor         = myPromptBorderColor
                 , maxComplRows        = Just 1
                 , showCompletionOnTab = False
                 }

centerWindow :: Window -> X ()
centerWindow win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  let newH | h > (23 / 24) = h - (1 / 24)
           | h < (3 / 4)   = (3 / 4)
           | otherwise     = h
  let newW | w > (3 / 4) = (3 / 4)
           | w < (1 / 2) = (1 / 2)
           | otherwise   = w
  windows
    $ W.float win (W.RationalRect ((1 - newW) / 2) ((1 - newH) / 2) newW newH)
  return ()

myWorkspaceKeys conf@(XConfig { XMonad.modMask = modMask }) =
  Map.fromList
    $  [ ((modMask, xK_Return)             , spawn myTerminal)
       , ((modMask, xK_p)                  , shellPrompt promptConf)
       , ((modMask, xK_b)                  , spawn myBrowser)
       , ((modMask, xK_v)                  , spawn myEditor)
       , ((modMask, xK_q)                  , spawn "lock")
       , ((modMask .|. shiftMask, xK_q)    , spawn "lock suspend")
       , ((modMask, xK_x), spawn "if type xmonad; then xmonad --restart; fi")
       , ((modMask .|. shiftMask, xK_x)    , io (exitWith ExitSuccess))
       , ((modMask .|. shiftMask, xK_c)    , kill)
       , ((modMask, xK_space)              , sendMessage NextLayout)
       , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ((modMask, xK_r)                  , sendMessage $ Rotate)
       , ((modMask, xK_a)                  , sendMessage $ FocusParent)
       , ((modMask .|. controlMask, xK_l)  , sendMessage $ ExpandTowards R)
       , ((modMask .|. controlMask, xK_h)  , sendMessage $ ExpandTowards L)
       , ((modMask .|. controlMask, xK_j)  , sendMessage $ ExpandTowards D)
       , ((modMask .|. controlMask, xK_k)  , sendMessage $ ExpandTowards U)
       , ((modMask, xK_l)                  , windowGo R False)
       , ((modMask, xK_h)                  , windowGo L False)
       , ((modMask, xK_k)                  , windowGo U False)
       , ((modMask, xK_j)                  , windowGo D False)
       , ((modMask .|. shiftMask, xK_l)    , windowSwap R False)
       , ((modMask .|. shiftMask, xK_h)    , windowSwap L False)
       , ((modMask .|. shiftMask, xK_k)    , windowSwap U False)
       , ((modMask .|. shiftMask, xK_j)    , windowSwap D False)
       , ((modMask, xK_u)                  , windows W.focusUp)
       , ((modMask, xK_i)                  , windows W.focusDown)
       , ((modMask, xK_s)                  , switchLayer)
       , ((modMask, xK_f)                  , withFocused centerWindow)
       , ((modMask, xK_t)                  , withFocused $ windows . W.sink)
       , ((modMask, xK_equal)              , sendMessage Equalize)
       , ((modMask, xK_minus)              , toggleWS)
       ]

    ++ [ ((m .|. modMask, key), f sc)
       | (key, sc) <- zip [xK_w, xK_e] [0 ..]
       , (f  , m ) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
       ]

    ++ [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

    ++ [ ((0, xF86XK_MonBrightnessUp)  , spawn "bri laptop up")
       , ((0, xF86XK_MonBrightnessDown), spawn "bri laptop down")
       , ((0, xF86XK_AudioRaiseVolume) , spawn "vol up")
       , ((0, xF86XK_AudioLowerVolume) , spawn "vol down")
       , ((0, xF86XK_AudioMute)        , spawn "vol mute")
       ]

stringifyLayout :: String -> String
stringifyLayout l | t "BSP" l  = fmt "bsp"
                  | t "Full" l = fmt "full"
                  | otherwise  = fmt l
 where
  t = List.isInfixOf
  fmt s = "[" ++ s ++ "]"

myLogHook h = do
  let fmt fg bg = D.pad . D.pad . D.xmobarColor fg bg
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
  $ lessBorders (OnlyScreenFloat) (emptyBSP ||| Full)
 where
  gs             = 5
  uniformSpacing = spacingRaw False (border) True (border) True
  border         = Border gs gs gs gs

myManageHook = composeAll [isDialog =? True --> doCenterFloat]

myNavConf = def { defaultTiledNavigation = sideNavigation }

myConfig pipe =
  withUrgencyHook NoUrgencyHook
    $ withNavigation2DConfig myNavConf
    $ ewmh
    $ docks def { logHook            = myLogHook pipe
                , manageHook         = myManageHook
                , layoutHook         = avoidStruts $ myLayoutHook
                , handleEventHook    = fullscreenEventHook
                , focusFollowsMouse  = False
                , workspaces         = myWorkspaces
                , terminal           = myTerminal
                , modMask            = myModMask
                , keys               = myWorkspaceKeys
                , borderWidth        = myBorderWidth
                , normalBorderColor  = myUnfocusedBorderColor
                , focusedBorderColor = myFocusedBorderColor
                }

main = xmonad . myConfig =<< spawnPipe "xmobar"

-- programs
myEditor = "editor -c"
myTerminal = "terminal"
myBrowser = "firefox"

-- colors
dark = "#46474d"
light = "#fcfcfc"
muted = "#828282"
foreground = "#ababb4"
primary = "#e8e8e8"
urgent = "#8b3800"
myFocusedBorderColor = "#46474d"
myUnfocusedBorderColor = "#a6a7ad"
myPromptBorderColor = "#26272d"

-- config vars
myBorderWidth = 2
myPromptBorderWidth = 1
myModMask = mod4Mask
myWorkspaces = ["def", "web", "dev", "misc"]
