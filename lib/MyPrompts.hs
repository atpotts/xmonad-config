module MyPrompts where

import Colors

import XMonad
import XMonad.Prompt
import Graphics.X11.Types

import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell


-- allWindows is of type X (Map String Window) - so we can change the name
myPromptKeyMap k  = promptMap k $ myPrompts myXPConfig

promptMap :: KeyMask -> [(KeyMask,KeySym,String,X ())] -> [((KeyMask,KeySym),X ())]
promptMap k ps = map reprompt ps
  where reprompt (m,s,_,x) = ((k .|. m,s),x)


myPrompts :: XPConfig -> [(KeyMask,KeySym, String, X ())]
myPrompts conf = [
          (noModMask, xK_a, "run", runOrRaisePrompt runConf),
          (noModMask, xK_w, "win", windowPrompt winConf Goto allWindows),
          (shiftMask, xK_w, "wgo", windowPrompt winConf BringToMaster allWindows),
          (noModMask, xK_s, " > ", shellPrompt shellConf),
          (shiftMask, xK_s, " >>", prompt ("urxvt -e") termConf)
          ]
  where shellConf = mkColor conf yellow
        termConf = mkColor conf red
        winConf = mkColor conf violet
        runConf = mkColor conf green

myXPConfig = defaultXPConfig {
           font = myFont,
           bgColor = yellow,
           fgColor = base3,
           fgHLight = base00,
           bgHLight = base3,
           position = Top,
           promptBorderWidth = 0,
           height = 32,
           changeModeKey = xK_Control_L,
           defaultText = ": "
      }

mkColor :: XPConfig -> String -> XPConfig
mkColor x c = x{bgColor = c}
