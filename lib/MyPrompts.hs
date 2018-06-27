{-# LANGUAGE TupleSections #-}
-- A prompt file
-- A number of prompts are allocated keybindings.
-- In addition, A general prompt can pull up the keybindings
-- for the whole list of prompts.

-- TODO: This was supposed to be a configuration, but has become a bit of a library
-- as well. I should really factor out the two components
module MyPrompts (
    myKeyMap,
    promptMap,
    fuzzysearch,
    boolfuzzysearch,
    XPConfig(..),
    XWindowMap,
    myXPConfig,
    mkColor,
    multiPrompt,
    motion,
    maybemod,
    expand,
    PromptList,
    WindowPrompt,
    myPrompts
  )
  where
  
import Colors

import Data.Char (toLower)
import Data.Function ((&))
import Data.List (intercalate, sort, sortOn, nub)
import Data.Char (isUpper, toLower)
import Control.Applicative (liftA2)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Graphics.X11.Types
import XMonad
import XMonad.Actions.Submap
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig

type PromptList = [([String],String,X ())]

-- allWindows is of type X (Map String Window) - so we can change the name
myKeyMap key conf promptlist = promptMap $
        (return key, "", multiPrompt conf promptlist ) : promptlist 


promptMap :: PromptList -> [(String,X ())]
promptMap ps = concatMap reprompt ps
  where reprompt (m,_,x) = map (,x) m



myPrompts :: XPConfig -> XWindowMap -> PromptList  
myPrompts conf  windows  = [
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
           -- changeModeKey = xK_Control_L,
           searchPredicate = boolfuzzysearch,
           alwaysHighlight = True
      }

mkColor :: XPConfig -> String -> XPConfig
mkColor x c = x{borderColor = c, fgHLight = c}

multiPrompt :: XPConfig -> PromptList -> X ()
multiPrompt conf list =
  inputPromptWithCompl newconf "" completions ?+ promptFromTitle
  where
    promptMap =
      M.fromList $
      map
        (\(key, title, prompt) ->
           ( title ++
             if not (null key)
               then " (" ++ intercalate ", " (sort key) ++ ")"
               else ""
           , prompt))
        list

    promptFromTitle t =
      case M.lookup t promptMap of
        Just a -> a
        _ -> return ()

    completions k =
      M.keys promptMap & map (\a -> (, a) <$> fuzzysearch 0 k a) & catMaybes &
      sortOn fst &
      map snd &
      return

    newconf =
      conf
        { promptKeymap =
            M.insert
              (noModMask, xK_space)
              (setSuccess True >> setDone True)
              (promptKeymap conf)
        }

motion :: XConfig l ->  [([String],String,Int -> X())] -> [([String],String,X())]
motion cf xs = go 0 ["M-"] xs
  where go n mask xs = map (\(a,b,c) -> (liftA2 (++) mask a,b,
                                  c (if n==0 then 1 else n))) xs
                  ++  [( map (++show i) mask
                       ,"Prefix "++show i
                       , submap . (mkKeymap cf) .
                          concatMap (\(a,_ ,c) -> map (,c) a) $
                            go (i+10*n) (nub ("":mask)) xs
                        ) | i <- [0..9] ]

maybemod :: String -> String
maybemod x@('<':_) = x
maybemod x@('M':'-':_) = x
maybemod xs = "M-"++ case xs of
  [a] -> if isUpper a then ['S','-',toLower a] else [a]
  xs' -> xs'

expand :: [String] -> [String]
expand xs = do
  x <- xs
  let q:qs = words x
  qs' <- sequence (map (\x -> nub [maybemod x,x]) qs)
  return . unwords $ maybemod q : qs'
