import XMonad
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Cross
-- import XMonad.Layout.LayoutModifier
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Maximize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_g     ), spawn "gmrun")
    , ((modMask,               xK_f     ), spawn "firefox")
    , ((modMask,               xK_o     ), spawn "geany ~/.xmonad/xmonad.hs")
    , ((modMask .|. shiftMask, xK_m     ), spawn "sudo shutdown -h now")
    , ((modMask,               xK_w     ), spawn "/usr/lib/wicd/gui.py")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_e     ), windows W.focusDown)
    , ((modMask,               xK_Right ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_Left  ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_e     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,               xK_backslash), withFocused (sendMessage . maximizeRestore))
    , ((modMask,               xK_x     ), shellPrompt defaultXPConfig)
    , ((modMask,               xK_b     ), spawn "bat | gdbar -ss 1  -h 10 -w 90 -fg 'orange' -bg '#222222' | dzen2 -p 3 -w 90 -h 10 -x 1188 -y 18")
    , ((modMask .|. shiftMask, xK_b     ), sendMessage ToggleStruts)
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask,               xK_q     ), spawn "killall dzen2 && killall conky" >> restart "xmonad" True)
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))	
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]
 
myLayout = avoidStruts (tabbed shrinkText myTabConfig ||| noBorders tall ||| Mirror tall ||| Grid ||| Full ||| simplestFloat ||| simpleCross)
  where
     tall   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 2/100

myTabConfig = defaultTheme {
activeColor = "#BBBFA1"
,inactiveColor = "#222222"
,urgentColor = "#ffffff"
,activeBorderColor = "#777777"
,inactiveBorderColor = "#777777"
,activeTextColor = "black"
,inactiveTextColor = "white"
,urgentTextColor = "orange"
,fontName = "xft:dina-12" 
,decoHeight = 21
} 

myManageHook = composeAll
			[ className =? "Gmrun"          --> doCenterFloat
			, className =? "Gran Paradiso"  --> doF (W.shift "^ca(1,ws2) C  ^ca()")
			, className =? "Gcolor2"        --> doCenterFloat
			, className =? "VLC (XVideo output)"   --> doFullFloat
			, className =? "Gimp"           --> doFloat
			, resource  =? "desktop_window" --> doIgnore]
 
myLogHook h = dynamicLogWithPP $ defaultPP {
			  ppCurrent  = dzenColor "black" "#BBBFA1" 
            , ppHidden   = dzenColor "#BBBFA1" "#222222" 
            , ppUrgent   = dzenColor "#D8332C" "#2E2611"
            , ppHiddenNoWindows = dzenColor "#444444"  "#222222"
            , ppWsSep    = "^fg(black)^r(1x17)"
            , ppSep      = "^fg(black)^r(1x17)"
            , ppLayout   = dzenColor "#BBBFA1" "#222222" .
                           (\ x -> fill (case x of
                       "Grid"		        -> icon "grid.xbm" 
                       "Tabbed Simplest"    -> icon "tabbed.xbm"
                       "Tall"               -> icon "tall.xbm"
                       "Mirror Tall"        -> icon "mtall.xbm"
                       "SimplestFloat"      -> icon "float.xbm"	
                       "Full"               -> icon "full.xbm"
                       _                    -> pad x) 4)
	    , ppTitle	= ("^fn(-*-dina-*-r-*-*-14-*-*-*-*-*-iso8859-1)^fg(#F6F7F1) " ++) . dzenEscape
        , ppOutput   = hPutStrLn h
            }
    where
      icon h = "^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      fill :: String -> Int -> String
      fill h i = "^ca(1,wss)^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")^ca()"
									 									      
myStatusBar = "dzen2 -fn 'Numberpile-12' -bg '#222222' -h 17 -ta l -w 800 -e ''"

secondDzenCommand = "conky -c ~/.xdzenconky | dzen2 -fn '-*-dina-*-r-*-*-14-*-*-*-*-*-iso8859-1' -bg '#222222' -h 17 -ta r -w 1280 -e '' -x 800"  

main = do din <- spawnPipe myStatusBar
	  spawnPipe secondDzenCommand
          
          xmonad $  withUrgencyHook NoUrgencyHook
					defaultConfig {
          terminal           = "urxvtc",
          focusFollowsMouse  = True,
          borderWidth        = 1,
          modMask            = mod1Mask,
          numlockMask        = mod2Mask,

          workspaces         = "^ca(1,ws1) B  ^ca()":"^ca(1,ws2) C  ^ca()":"^ca(1,ws3) D  ^ca()":"^ca(1,ws4) E  ^ca()":"^fn(Bands & Artists-11)^ca(1,ws5) j  ^ca()":[],
--          workspaces         = " B  ":" C  ":" D  ":" E  ":" j  ":[],
          normalBorderColor  = "#BBBFA1",
          focusedBorderColor = "white",        
     
          -- key bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,
 
          -- hooks, layouts
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          logHook            = myLogHook din     
          }
			  --ppVisible = f

              --ws = ["A", "B", "C"]

              --f s = "^ca(1,ws" ++ (show i) ++ ")" ++ s ++ "^ca()"
                 --where
                   --i = fromJust $ elemIndex s ws
      --cap s i = "^ca(1,ws" ++ (show i) ++ ")" ++ s ++ "^ca()"
	          --where
			    --ws = " B  ":" C  ":" D  ":" E  ":" j  ":[]    
                   --i = fromJust $ elemIndex s ws
