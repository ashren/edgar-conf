import XMonad hiding ((|||))
import XMonad.ManageHook
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.Promote

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.DwmStyle
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Data.Ratio ((%))

statusBarCmd= "dzen2 -e '' -w 1400 -ta l -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*' -bg black -fg #d3d7cf "

main = do
       din <- spawnPipe statusBarCmd
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn", "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*","-bg", "yellow", "-fg", "red"] } $ defaultConfig
                     { borderWidth        = 3
                     , workspaces         = ["1:main","2:web","3:dev","4:im"] ++ map show [5..9]
                     , terminal           = "terminator"
                     , modMask            = mod4Mask
                     , manageHook         = myManageHook <+> manageHook defaultConfig <+> manageDocks <+> scratchpadManageHookDefault
                     , logHook            = dynamicLogWithPP $ myPP din
                     , layoutHook         = myLayouts
                     }
                     `additionalKeysP` myKeys din

myManageHook  = composeAll [ className =? "Pidgin"         --> doF (W.shift "2:im")
                           , className =? "Firefox"        --> doF (W.shift "3:web")
                           , className =? "Chromium"  --> doF (W.shift "3:web")
                           , title     =? "alpine"           --> doF (W.shift "4:mail")
                           ]

myKeys conf = [ ("M-<Return>", spawn "terminator")
              , ("M-p",        spawn "dmenu_run")
              , ("M-c",        kill)
              -- run programs
              , ("M-f",        spawn "firefox")
              , ("M-e",        spawn "thunar")
              , ("M-s",        scratchpadSpawnActionTerminal "terminator")
              -- resize tile
              , ("M-a",        sendMessage MirrorShrink)
              , ("M-z",        sendMessage MirrorExpand)
              -- moving workspaces
              , ("M-<Left>",    prevWS)
              , ("M-<Right>",   nextWS)
              , ("M-S-<Left>",  shiftToPrev)
              , ("M-S-<Right>", shiftToNext)
              , ("M-<Tab>",     toggleWS)

              , ("M-S-<Return>", promote)

              , ("M-u", focusUrgent)
              ]

myPP h = defaultPP
                 {  ppCurrent = wrap "^fg(#000000)^bg(#a6c292) " " ^fg()^bg()"
                  , ppHidden  = wrap "^i(/home/meatcar/.dzen/bitmaps/rob/has_win_nv.xbm)" " "
                  , ppHiddenNoWindows  = wrap " " " "
                  , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
                  , ppWsSep   = ""
                  , ppLayout  = dzenColor "#80AA83" "" .
                                (\x -> case x of
                                         "Tall"  -> "^i(/home/meatcar/.dzen/bitmaps/rob/tall.xbm)"
                                         "Mirror" -> "^i(/home/meatcar/.dzen/bitmaps/rob/mtall.xbm)"
                                         "Tabs" -> "Tabs"
                                         "IM"  -> "IM"
                                )
                  , ppTitle   = dzenColor "white" "" . wrap "< " " >"
                  , ppOutput  = hPutStrLn h
                  , ppUrgent = dzenColor "yellow" "red" . dzenStrip
                  }

myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

myLayouts = avoidStruts $ smartBorders $
  onWorkspace "2:im" (named "IM" (reflectHoriz $ withIM (1%8) (Title "Buddy List") (reflectHoriz $ dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs)))) $
  onWorkspace "3:web" (tabs) $
  (tiled ||| named "Mirror" (Mirror tiled) ||| tabs)
    where
      tiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
      tabs = named "Tabs" (tabbed shrinkText myTheme)
