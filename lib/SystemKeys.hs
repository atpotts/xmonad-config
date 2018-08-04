module SystemKeys where

import XMonad
import MyPrompts


systemKeys = volumeKeys ++ brightnessKeys

data ValChange = Inc
               | Dec
               | Toggle

volume :: ValChange -> X ()
volume a = amix $ case a of
    Inc    -> "2000+"
    Dec    -> "2000-"
    Toggle -> "toggle"
  where
     amix = spawn . ("amixer -D pulse set Master " ++)

volumeKeys =
 [ Action ["<XF86AudioLowerVolume>"] "Vol-" ( volume Dec)
 , Action ["<XF86AudioRaiseVolume>"] "Vol+" ( volume Inc)
 , Action ["<XF86AudioMute>"] "Mute" ( volume Toggle)
 ]

brightness :: ValChange -> X ()
brightness a = case a of
   Inc -> xbacklight "+5"
   Dec -> xbacklight "-5"
   _   -> return ()
 where xbacklight = spawn . ("xbacklight -inc "++)

brightnessKeys = 
 [ Action ["<XF86MonBrightnessUp>"] "Screen Brightness Up" ( brightness Inc)
 , Action ["<XF86MonBrightnessDown>"] "Screen Brightness Down" ( brightness Dec)
 ]
