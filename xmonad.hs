{-# LANGUAGE
  OverloadedStrings
#-}

import MyPrompts (myPromptKeyMap)
import Colors
import WindowNames

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run       (spawnPipe)
import XMonad.Util.EZConfig  (additionalKeys)

import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.TwoPane

import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W

import Data.Monoid           ((<>))
import System.IO
import Data.List (isInfixOf)
import qualified Data.Map as M

-- Layout Names
myLayout =  tall ||| twopane ||| wide ||| full
  where 
    incstep = 3/100
    startdivision = 1/2
    spc = spacing 10
    tall    = spc $ Tall 1 incstep startdivision
    twopane = spc $ TwoPane incstep startdivision
    wide    = spc $ Mirror tall
    full    = spc $ Full

layoutformatter s
    | "Mirror" `isInfixOf` s = "━┯━"
    | "Full" `isInfixOf` s   = "   "
    | "TwoPane" `isInfixOf` s = " ┃ "
    | otherwise              = " ┠─"	

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

          ((cmdkey,                xK_d),      kill),                       
          ((cmdkey,                xK_Return), sendMessage NextLayout),     
          -- ((cmdkey .|. shiftMask , xK_Return), sendMessage PreviousLayout), 

          -- free xK_return
          ((cmdkey,               xK_m), windows W.focusMaster),
          ((cmdkey .|. shiftMask, xK_m), windows W.swapMaster)


        ] ++ myPromptKeyMap cmdkey



runXmobar = spawnPipe $  home ".nix-profile/bin/xmobar"
                     /./ home ".xmobarrc"

myLogTitle :: Logger
myLogTitle = withWindowSet $ traverse (anyWindowNamer Nothing) . W.peek

xmobarHook xmproc = dynamicLogWithPP xmobarPP
                          { ppOutput          = hPutStrLn xmproc
                          , ppCurrent         = \n ->
                                xmobarColor base3 (accentcolors n) $ pad n
                          , ppVisible         = xmobarColor base0 inherit .
                                (\x -> "["<> x <>"]")
                          , ppHidden          = xmobarColor base0 inherit . pad
                          , ppHiddenNoWindows = const ""
                          , ppLayout = xmobarColor base2 base1 . layoutformatter
                          , ppTitle = const ""
                          , ppExtras = [
                                fmap (fmap
                                        (xmobarColor base0 base2 . shorten 50))
                                     myLogTitle
                                ]
                          , ppSep = " "
                          , ppWsSep = ""
                          , ppOrder = \(layout : workspaces : title : rest) ->
                                        workspaces : layout : rest
                          }

home x = "/home/alistairtpotts/" <> x
