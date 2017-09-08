{-# LANGUAGE
  OverloadedStrings
#-}

import MyPrompts
import Colors

import XMonad
import Data.Monoid           ((<>))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Tabbed

import XMonad.Util.Run       (spawnPipe)
import XMonad.Util.EZConfig  (additionalKeys)
import XMonad.Util.Dmenu

import XMonad.Prompt
import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W
import System.IO
import Data.List (isInfixOf)
import qualified Data.Map as M

-- Layout Names
myLayout =  tall ||| wide ||| full
  where 
    spc = spacing 10
    tall = spc $ Tall 1 (3/100) (1/2)
    wide = spc $ Mirror tall
    full = spc $ Full

layoutformatter s
    | "Mirror" `isInfixOf` s = "─┬─"
    | "Full" `isInfixOf` s   = "   "
    | otherwise              = " ├─"	

-- key overrides
cmdkey = mod3Mask




a /./ b = a <> " " <> b
-- for command line arguments
a /=/ b = a <> " " <> "\"" <> b <> "\""

managementHooks :: [ManageHook]
managementHooks = [
    resource =? "stalonetray" --> doIgnore
    ]

main = do
    xmproc <- runXmobar
    xmonad $ docks defaultConfig {
          terminal   = "urxvt",
          manageHook = manageDocks <+> manageHook defaultConfig
                                   <+> composeAll managementHooks,
          layoutHook = avoidStruts myLayout,
          logHook    = xmobarHook xmproc,
          -- use Mod key
          modMask = cmdkey,

          -- Borders
          borderWidth = 3,
          normalBorderColor = base2,
          focusedBorderColor = base1
          -- keys = myKeys
        }
        `additionalKeys` myKeys

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
           -- ((cmdkey,xK_s),
           --      spawn $ "dmenu_run"
           --          -- /./"-l 0"                   -- 1 line (/./ -b bottom)
           --          /./"-i"                     -- case insesnitive
           --          /./"-fn \"Meslo LG L DZ-11\"" -- font
           --          /./"-nb"  /=/ base2        -- bg
           --          /./"-nf"  /=/ base0         -- foreground
           --          /./"-p \">\""               -- prompt
           --          /./"-sb"  /=/ yellow        -- selected background
           --          /./"-sf"  /=/ base3        -- selected foreground
           --          ),

          ((cmdkey,xK_d), kill),
          ((cmdkey, xK_backslash),sendMessage NextLayout)
                
        ] ++ myPromptKeyMap cmdkey


-- myKeys conf = M.fromList $
--     [ ((cmdkey, xK_x               ), kill                                ) -- close window
--     , ((cmdkey,  xK_q              ), restart "xmonad" True               )
--     , ((cmdkey, xK_m               ), nextWS                              ) -- move to next workspace
--     , ((cmdkey, xK_n               ), prevWS                              ) -- move to prev workspace
--     , ((cmdkey .|. mod1Mask, xK_m  ), shiftToNext                         ) -- move window to next workspace
--     , ((cmdkey .|. mod1Mask, xK_n  ), shiftToPrev                         ) -- move window to prev workspace
--     , ((cmdkey, xK_o               ), windows W.swapMaster                ) -- push to master
--     , ((cmdkey, xK_i               ), windows W.focusMaster               )
--     , ((cmdkey, xK_h               ), sendMessage Shrink                  ) -- shrink the master area
--     , ((cmdkey, xK_comma           ), sendMessage (IncMasterN 1         ) )
--     , ((cmdkey, xK_period          ), sendMessage (IncMasterN (-1     ) ) )
--     , ((cmdkey, xK_j               ), windows W.focusDown                 ) -- swap the focused window with the next window
--     , ((cmdkey, xK_0               ), (setLayout $ XMonad.layoutHook conf ) ) --Reset layout to workspaces default
--     , ((cmdkey, xK_k               ), windows W.focusUp                   )  -- swap thefocused window with the previous window
--     , ((cmdkey, xK_l               ), sendMessage Expand                  ) -- expand the master area
--     , ((cmdkey .|. shiftMask, xK_q ), io (exitWith ExitSuccess          ) ) -- quit xmonad
--     , ((cmdkey .|. shiftMask, xK_t ), spawn "urxvtc"                      )
--     ]  

runXmobar = spawnPipe $  home ".nix-profile/bin/xmobar"
                     /./ home ".xmobarrc"

xmobarHook xmproc = dynamicLogWithPP xmobarPP
                          { ppOutput          = hPutStrLn xmproc
                          , ppCurrent         = \n ->
                                xmobarColor base3 (accentcolors n) $ pad n
                          , ppVisible         = xmobarColor base0 inherit .
                                (\x -> "["<> x <>"]")
                          , ppHidden          = xmobarColor base0 inherit . pad
                          , ppHiddenNoWindows = const ""
                          , ppLayout = xmobarColor base2 base1 . layoutformatter
                          , ppTitle = xmobarColor base0 base2 . shorten 50
                          , ppSep = " "
                          , ppWsSep = ""
                          , ppOrder = \(layout : workspaces : rest) ->
                                        workspaces : layout : rest
                          }

home x = "/home/alistairtpotts/" <> x
