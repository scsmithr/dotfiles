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

promptConf = def { position          = Bottom
                 , font              = "xft:Fira Sans Medium-10"
                 , height            = 44
                 , bgColor           = light
                 , fgColor           = muted
                 , bgHLight          = light
                 , fgHLight          = dark
                 , promptBorderWidth = 1
                 , borderColor       = myUnfocusedBorderColor
                 , maxComplRows      = Just 3
                 }

myWorkspaceKeys conf@(XConfig { XMonad.modMask = modMask }) =
  Map.fromList
    $  [ ((modMask, xK_Return), spawn myTerminal)
       , ((modMask, xK_p)     , shellPrompt promptConf)
       , ((modMask, xK_b)     , spawn myBrowser)
       , ((modMask, xK_v)     , spawn myEditor)
       , ((modMask, xK_q)     , spawn "lock")
       , ( (modMask .|. shiftMask, xK_q)
         , spawn "lock suspend"
         )

     -- switch layouts
       , ((modMask .|. shiftMask, xK_c), kill)
       , ((modMask, xK_space)          , sendMessage NextLayout)
       , ( (modMask .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

    -- move focus up or down the window stack
       , ((modMask, xK_Tab)              , windows W.focusDown)
       , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)
       , ((modMask, xK_j)                , windows W.focusDown)
       , ((modMask, xK_k)                , windows W.focusUp)
       , ( (modMask, xK_m)
         , windows W.focusMaster
         )

    -- modifying the window order
       , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
       , ((modMask .|. shiftMask, xK_j)     , windows W.swapDown)
       , ( (modMask .|. shiftMask, xK_k)
         , windows W.swapUp
         )

    -- resizing the master/slave ratio
       , ((modMask, xK_h), sendMessage Shrink)
       , ( (modMask, xK_l)
         , sendMessage Expand
         )

    -- floating layer support
       , ( (modMask, xK_t)
         , withFocused $ windows . W.sink
         )

    -- increase or decrease number of windows in the master area
       , ((modMask, xK_comma), sendMessage (IncMasterN 1))
       , ( (modMask, xK_period)
         , sendMessage (IncMasterN (-1))
         )

     -- quickly jump back to prev workspace
       , ((modMask, xK_minus), toggleWS)
       ]

    -- navigate screens based on physical position
    ++ [ ((m .|. modMask, key), f sc)
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f  , m ) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
       ]

    ++ [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

  -- media keys
    ++ [ ((0, xF86XK_MonBrightnessUp)  , spawn "bri laptop up")
       , ((0, xF86XK_MonBrightnessDown), spawn "bri laptop down")
       , ((0, xF86XK_AudioRaiseVolume) , spawn "vol up")
       , ((0, xF86XK_AudioLowerVolume) , spawn "vol down")
       , ((0, xF86XK_AudioMute)        , spawn "vol mute")
       ]

stringifyLayout :: String -> String
stringifyLayout l | t "Tall" l     = fmt "tall"
                  | t "Full" l     = fmt "full"
                  | t "ThreeCol" l = fmt "col"
                  | otherwise      = fmt l
 where
  t = List.isInfixOf
  fmt s = "[" ++ s ++ "]"

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

myConfig pipe = withUrgencyHook NoUrgencyHook $ ewmh $ docks def
  { logHook            = myLogHook pipe
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
myEditor = "emacs"
myTerminal = "terminal"
myBrowser = "firefox"

-- colors
dark = "#26272d"
light = "#fcfcfc"
muted = "#828282"
foreground = "#ababb4"
primary = "#e8e8e8"
urgent = "#8b3800"
myFocusedBorderColor = "#93a4a6"
myUnfocusedBorderColor = "#c6d3d3"

-- config vars
myBorderWidth = 2
myModMask = mod4Mask
myWorkspaces = ["def", "email", "web", "dev"]
