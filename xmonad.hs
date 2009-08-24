import XMonad
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Hooks.XPropManage
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Maximize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
import XMonad.Actions.GridSelect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_g     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_x     ), spawn "sudo shutdown -h now")
    , ((modMask,               xK_f     ), spawn "firefox")
    , ((modMask,               xK_o     ), spawn "geany ~/.xmonad/xmonad.hs")
    , ((modMask .|. shiftMask, xK_m     ), spawn "sudo shutdown -h now")
    , ((modMask,               xK_w     ), spawn "/usr/lib/wicd/gui.py")
	, ((modMask,               xK_z     ), goToSelected defaultGSConfig)
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
    , ((modMask,               xK_b     ), spawn "bat")
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

		 
appFontXft :: String
appFontXft = concat [ "xft:"
                     ,"Sans:"
					 ,"pixelsize=11:"
					 ,"weight=regular:"
					 ,"width=semicondensed:"
					 ,"dpi=96:hinting=true:"
					 ,"hintstyle=hintslight:"
					 ,"antialias=true:"
					 ,"rgba=rgb:"
					 ,"lcdfilter=lcdlight"]
					 
dzenAppFontXft :: String
dzenAppFontXft = concat [ "'^fn("
                     ,"xft:"
                     ,"Sans:"
					 ,"pixelsize=11:"
					 ,"weight=regular:"
					 ,"width=semicondensed:"
					 ,"dpi=96:hinting=true:"
					 ,"hintstyle=hintslight:"
					 ,"antialias=true:"
					 ,"rgba=rgb:"
					 ,"lcdfilter=lcdlight)"
					 ,"^fg(#F6F7F1)'"]
					 
--["xft", "Sans", "pixelsize=11", "weight=regular", "width=semicondensed", "dpi=96", "hinting=true", "hintstyle=hintslight", "antialias=true", "rgba=rgb", "lcdfilter=lcdlight"]

-- appFont = zipWith (++) (ma#00D8FFp show [1..]) fntname
--   where fntname = map ((:) ':') appFontXft
	  
tabbedLayout = tabbedBottomAlways shrinkText myTabConfig
gimpLayout = named "gimp" (tabbedLayout ****||* Full)

myLayout = avoidStruts $ 
    (tabbedLayout ||| tall ||| Mirror tall ||| Full ||| simplestFloat ||| gimpLayout)
     where
         tall      = Tall nmaster delta ratio
         nmaster   = 1
         ratio     = 1/2
         delta     = 2/100
--myLayout = onWorkspace (myWorkspaces !! 3) gimpLayout $
--             myLayouts  

myTabConfig = defaultTheme {
activeColor = "#806E62"
,inactiveColor = "#CDD4B2"
,urgentColor = "#ffffff"
,activeBorderColor = "#777777"
,inactiveBorderColor = "#777777"
,activeTextColor = "black"
,inactiveTextColor = "white"
,urgentTextColor = "orange"
,fontName = appFontXft
-- "xft:Sans:pixelsize=11:weight=regular:width=semicondensed:dpi=96:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdlight"
,decoHeight = 21
} 


myWorkspaces            :: [String]			 									      
myWorkspaces            = clickable . (map dzenEscape) $ nWorkspaces 0 [ "B","C","D","E","F"]

  where nWorkspaces n []= map show [1 .. n]
        nWorkspaces n l = init l ++ map show [length l .. n] ++ [last l] 
        clickable l     = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = if i == 0 then 0 else i ] -- needed for 10 workspaces
myManageHook = composeAll
			[ className =? "Gmrun"          --> doCenterFloat
			, className =? "Shiretoko"  --> doF (W.shift (myWorkspaces !! 1))
			, className =? "Gcolor2"        --> doCenterFloat
			, className =? "VLC (XVideo output)"   --> doFullFloat
			, className =? "Gimp"  --> doF (W.shift (myWorkspaces !! 3)) 
			, resource  =? "desktop_window" --> doIgnore
			]
          where role = stringProperty "WM_WINDOW_ROLE"

myLogHook h = dynamicLogWithPP $ defaultPP {
-- dzenColor "black" "#CDD4B2"
-- dzenColor "#CDD4B2" "#222222" . pad 
			  ppCurrent  = dzenColor "#806E62" "#CDD4B2"  . pad
            , ppHidden   = dzenColor "#CDD4B2" "#806E62"  . pad
            , ppUrgent   = dzenColor "#D8332C" "#2E2611" . pad
            , ppHiddenNoWindows = dzenColor "#444444"  "#806E62" . pad
            , ppWsSep    = "^fg(black)^r(1x17)"
            , ppSep      = "^fg(black)^r(1x17)"
            , ppLayout   = dzenColor "#CDD4B2" "#806E62" .
                           (\ x -> fill (case x of
                       "Tabbed Bottom Simplest"    -> icon "tabbed.xbm"
                       "Tall"               -> icon "tall.xbm"
                       "Mirror Tall"        -> icon "mtall.xbm"
                       "SimplestFloat"      -> icon "float.xbm"	
                       "Full"               -> icon "full.xbm"
                       "gimp"               -> icon "full.xbm"
                       _                    -> pad x) 4)
	        , ppTitle	= ("^fn(xft:Sans:pixelsize=11:weight=regular:width=semicondensed:dpi=96:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdlight)^fg(#F6F7F1) " ++) . dzenEscape
            , ppOutput   = hPutStrLn h
            }
    where
      icon h = "^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      fill :: String -> Int -> String
      fill h i = "^ca(1,xdotool key alt+space)^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")^ca()"
-- Numberpile-12
myStatusBar = "dzen2 -fn 'Numberpile-12' -bg '#806E62' -h 17 -ta l -w 800 -e ''"
-- myStatusBar = "dzen2 -ta l -w 800"
-- secondDzenCommand = "date | dzen2 -ta r -x 800 -p"
secondDzenCommand = "conky -c ~/.xdzenconky | dzen2 -fn " ++ appFontXft ++ " -bg '#806E62' -h 17 -ta r -w 1280 -e '' -x 800"  
-- -*-dina-*-r-*-*-14-*-*-*-*-*-iso8859-1
main = do din <- spawnPipe myStatusBar
	  spawnPipe secondDzenCommand     
          xmonad $  withUrgencyHook dzenUrgencyHook { args = [ "-fn", "Dina-10", "-x", "420", "-w", "350", "-fg", "orange", "-bg", "#222222" ] }
					defaultConfig {
          terminal           = "urxvtc",
          focusFollowsMouse  = True,
          borderWidth        = 1,
          modMask            = mod1Mask,
          numlockMask        = mod2Mask,

          workspaces         = myWorkspaces,
          normalBorderColor  = "#CDD4B2",
          focusedBorderColor = "white",        
     
          -- key bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,
 
          -- hooks, layouts
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          logHook            = myLogHook din     
          }
