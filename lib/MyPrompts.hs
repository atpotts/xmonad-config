{-# LANGUAGE TupleSections #-}
-- A prompt file
-- A number of prompts are allocated keybindings.
-- In addition, A general prompt can pull up the keybindings
-- for the whole list of prompts.

-- TODO: This was supposed to be a configuration, but has become a bit of a library
-- as well. I should really factor out the two components
module MyPrompts where

import Colors

import XMonad
import XMonad.Prompt
import Graphics.X11.Types

import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
-- import XMonad.Prompt.Workspace
import XMonad.Layout.WorkspaceDir

import qualified Data.Map as M
import Data.Char (toLower)
import Data.Maybe (isJust,catMaybes)
import Data.List (sortOn,intercalate,sort)
import Data.Function ((&))

type PromptList = [([String],String,X ())]

-- allWindows is of type X (Map String Window) - so we can change the name
myKeyMap key conf promptlist = promptMap $
        (return key, "", multiPrompt conf promptlist ) : promptlist 


promptMap :: PromptList -> [(String,X ())]
promptMap ps = concatMap reprompt ps
  where reprompt (m,_,x) = map (,x) m


myPrompts :: XPConfig -> XWindowMap -> PromptList  
myPrompts conf  windows  = [
          (["M-a"], "Run or Raise",
                      runOrRaisePrompt runConf),
          (["M-w"], "Go to window",
                      windowPrompt winConf Goto windows),
          (["M-S-w"], "Bring window to master",
                      windowPrompt winConf BringToMaster windows),
          ([], "Bring window",
                      windowPrompt winConf Bring windows),
          ([], "Bring a copy",
                      windowPrompt winConf BringCopy windows),
          (["M-s"], "Run (sh)",
                      shellPrompt shellConf),
          (["M-S-s"], "Run (term)",
                      prompt "urxvt -e" termConf)
          ]
  where shellConf = mkColor nohlconf yellow
        termConf  = mkColor nohlconf red
        winConf   = mkColor conf violet
        runConf   = mkColor conf green
        nohlconf = conf {alwaysHighlight = False}

fuzzysearch :: Double -> String -> String -> Maybe Double
fuzzysearch n [] _ = Just 0
fuzzysearch n (q:qs) (x:xs)
    | toLower x == toLower q = ((sqrt n)+) <$> fuzzysearch 0 qs xs
    | otherwise              = fuzzysearch (n+1) (q:qs) (xs)
fuzzysearch _ _ _ = Nothing

boolfuzzysearch :: String -> String -> Bool
boolfuzzysearch a b = isJust $ fuzzysearch 0 a b

myXPConfig = def {
           font = myFont,
           bgColor = base2,
           fgColor = base0,
           fgHLight = base03,
           bgHLight = base2,
           borderColor=base0,
           position = CenteredAt 0.3 0.5,
           promptBorderWidth = 2,
           height = 40,
           maxComplRows = Just 10,
           changeModeKey = xK_Control_L,
           searchPredicate = boolfuzzysearch,
           alwaysHighlight = True
      }

mkColor :: XPConfig -> String -> XPConfig
mkColor x c = x{borderColor = c, fgHLight = c}

multiPrompt :: XPConfig -> PromptList -> X ()
multiPrompt conf list = inputPromptWithCompl newconf "" 
                    completions ?+ promptFromTitle

        where promptMap = M.fromList
                      $ map (\(key,title,prompt)->
                              (title++if not (null key)
                                      then " ("++intercalate ", " (sort key)++")"
                                      else "",
                                prompt)) list

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
                
                                               

