{-# LANGUAGE TupleSections #-}
-- A prompt file
-- A number of prompts are allocated keybindings.
-- In addition, A general prompt can pull up the keybindings
-- for the whole list of prompts.

module MyPrompts where

import Colors
import WindowNames

import XMonad
import XMonad.Prompt
import Graphics.X11.Types

import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
-- import XMonad.Prompt.Workspace

import qualified Data.Map as M
import Data.Char (toLower)
import Data.Maybe (isJust,catMaybes)
import Data.List (sortOn)
import Data.Function ((&))

-- allWindows is of type X (Map String Window) - so we can change the name
myPromptKeyMap k = promptMap k
               $ myPrompts myXPConfig windowNames

promptMap :: KeyMask -> [(KeyMask,KeySym,String,X ())] -> [((KeyMask,KeySym),X ())]
promptMap k ps = map reprompt ps
  where reprompt (m,s,_,x) = ((k .|. m,s),x)


myPrompts :: XPConfig -> XWindowMap -> [(KeyMask,KeySym, String, X ())]
myPrompts conf  windows = [
          (noModMask, xK_semicolon, "",
                      multiPrompt conf $ myPrompts conf windows),
          (noModMask, xK_a, "Run or Raise",
                      runOrRaisePrompt runConf),
          (noModMask, xK_w, "Go to Window",
                      windowPrompt winConf Goto windows),
          (shiftMask, xK_w, "Bring window to master",
                      windowPrompt winConf BringToMaster windows),
          (noModMask, xK_VoidSymbol, "Bring window",
                      windowPrompt winConf Bring windows),
          (noModMask, xK_VoidSymbol, "Bring a copy",
                      windowPrompt winConf BringCopy windows),
          (noModMask, xK_s, "Run (sh)",
                      shellPrompt shellConf),
          (shiftMask, xK_s, "Run (term)",
                      prompt ("urxvt -e") termConf)
          ]
  where shellConf = mkColor nohlconf yellow
        termConf  = mkColor nohlconf red
        winConf   = mkColor conf violet
        runConf   = mkColor conf green
        nohlconf = conf {alwaysHighlight = False}


fuzzysearch :: Int -> String -> String -> Maybe Int
fuzzysearch n [] _ = Just 0
fuzzysearch n (q:qs) (x:xs)
    | toLower x == toLower q = (n+) <$> fuzzysearch (n+1) qs xs
    | otherwise              = fuzzysearch (n+1) (q:qs) (xs)
fuzzysearch _ _ _ = Nothing

boolfuzzysearch :: String -> String -> Bool
boolfuzzysearch a b = isJust $ fuzzysearch 0 a b

myXPConfig = def {
           font = myFont,
           bgColor = base2,
           fgColor = base0,
           fgHLight = base3,
           bgHLight = base00,
           position = Top,
           promptBorderWidth = 0,
           height = 40,
           changeModeKey = xK_Control_L,
           searchPredicate = boolfuzzysearch,
           alwaysHighlight = True
      }

mkColor :: XPConfig -> String -> XPConfig
mkColor x c = x{fgColor = c, bgHLight = c}

multiPrompt :: XPConfig -> [(KeyMask,KeySym, String, X ())] -> X ()
multiPrompt conf list = inputPromptWithCompl newconf "" 
                    completions ?+ promptFromTitle

        where promptMap = M.fromList
                                $ map (\(_,_,title,prompt)->(title,prompt)) list

              promptFromTitle t = case M.lookup t promptMap of
                              Just a -> a
                              _ -> return ()

              completions k = M.keys promptMap
                   & map (\a -> (,a) <$> fuzzysearch 0 k a) 
                   & catMaybes
                   & sortOn fst
                   & map snd
                   & return

              newconf = conf {promptKeymap =
                                M.insert (noModMask, xK_space)
                                         (setSuccess True >> setDone True)
                                         (promptKeymap conf)
                              }
                
                                               

