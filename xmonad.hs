import XMonad
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Layout.NoBorders -- (smartBorders)
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Hooks.XPropManage
import XMonad.Actions.GridSelect
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Maximize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
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
    , ((modMask .|. shiftMask, xK_m     ), spawn "sudo reboot")
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

-- My colors and fonts

dzenColor1 :: String
dzenColor2 :: String
dzenColor3 :: String
dzenColor4 :: String
dzenColor5 :: String
dzenUrgent :: String

--dzenColor1 = "#F5F6F6" -- almost white
--dzenColor2 = "#D0EFF3" -- very light blue
--dzenColor3 = "#84D1E0" -- light blue
--dzenColor4 = "#4690BB" -- blue
--dzenColor5 = "#0D171A" -- dark
--dzenUrgent = "orange" -- pref. urgent color

dzenColor1 = "#8D7D5B" -- greyish
dzenColor2 = "#F9BD81" -- skin 
dzenColor3 = "#794A3A" -- brownish
dzenColor4 = "#D84739" -- reddish 
dzenColor5 = "#543532" -- dark
dzenUrgent = "orange" -- pref. urgent color

appFontXft0 :: String
appFontXft0 = "saxMono:pixelsize=12"

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

-- Layout Section

tabbedLayout = tabbedBottomAlways shrinkText myTabConfig
gimpLayout = named "gimp" (tabbedLayout ****||* noBorders Full)

myLayout = avoidStruts  $ 
    (noBorders tabbedLayout ||| noBorders tall ||| Mirror tall ||| noBorders Full ||| noBorders simplestFloat ||| gimpLayout)
     where
         tall      = Tall nmaster delta ratio
         nmaster   = 1
         ratio     = 1/2
         delta     = 2/100


myTabConfig = defaultTheme {
activeColor = dzenColor2
,inactiveColor = dzenColor5
,urgentColor = "black"
,activeBorderColor = dzenColor4
,inactiveBorderColor = "#594F4F"
,activeTextColor = dzenColor5
,inactiveTextColor = dzenColor1
,urgentTextColor = dzenUrgent
,fontName = appFontXft
,decoHeight = 21
} 

-- Ws change by leftclick conf starts here:
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ nWorkspaces 0 ["Turn on","Tune in","and","Drop","Out"]

  where nWorkspaces n []= map show [1 .. n]
        nWorkspaces n l = init l ++ map show [length l .. n] ++ [last l] 
        clickable l     = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = if i == 0 then 0 else i ]
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
			  ppCurrent  = dzenColor dzenColor3 dzenColor2  . pad
            , ppHidden   = dzenColor dzenColor4 dzenColor5 . pad
            , ppUrgent   = dzenColor dzenColor2 dzenColor4 . pad
            , ppHiddenNoWindows = dzenColor dzenColor1 dzenColor5 . pad
            , ppWsSep    = "^fg(black)^r(1x17)"
            , ppSep      = "^fg(black)^r(1x17)"
            , ppLayout   = dzenColor dzenColor4 dzenColor5 .
                           (\ x -> fill (case x of
                       "Tabbed Bottom Simplest"    -> icon "tabbed.xbm"
                       "Tall"               -> iconb "tall.xbm"
                       "Mirror Tall"        -> iconc "mtall.xbm"
                       "SimplestFloat"      -> icond "float.xbm"
                       "Full"               -> icone "full.xbm"
                       "gimp"               -> iconf "gimp.xbm"
                       _                    -> pad x) 4)
	        , ppTitle	= ("^fn(xft:Sans:pixelsize=11:weight=regular:width=semicondensed)^fg(#F9BD81) " ++) . dzenEscape
            , ppOutput   = hPutStrLn h
            }
    where
      iconb h = "^fg(blue)^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      iconc h = "^fg(yellow)^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      icond h = "^fg(white)^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      icone h = "^fg(purple)^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      iconf h = "^fg(green)^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      icon h = "^i(/home/edgar/dzen_bitmaps/" ++ h ++ ")"
      fill :: String -> Int -> String
      fill h i = "^ca(1,xdotool key alt+space)^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")^ca()"

myStatusBar = "dzen2 -fn " ++ appFontXft ++ " -bg '#543532' -h 17 -ta l -w 800 -e ''"
secondDzenCommand = "conky -c ~/.xdzenconky | dzen2 -fn " ++ appFontXft ++ " -bg '#543532' -h 17 -ta r -w 1280 -e '' -x 800"  

main = do din <- spawnPipe myStatusBar
	  spawnPipe secondDzenCommand     
          xmonad $  withUrgencyHook dzenUrgencyHook { args = [ "-fn",  "Dina-10" , "-x", "420", "-w", "350", "-fg", "orange", "-bg", "#222222" ] }
					defaultConfig {
          terminal           = "urxvtc",
          focusFollowsMouse  = True,
          borderWidth        = 1,
          modMask            = mod1Mask,
          numlockMask        = mod2Mask,

          workspaces         = myWorkspaces,
          normalBorderColor  = dzenColor2,
          focusedBorderColor = dzenColor2,        
     
          -- key bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,
 
          -- hooks, layouts
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          logHook            = myLogHook din     
          }
