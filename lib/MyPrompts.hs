{-# LANGUAGE TupleSections #-}
-- A prompt file
-- A number of prompts are allocated keybindings.
-- In addition, A general prompt can pull up the keybindings
-- for the whole list of prompts.

-- TODO: This was supposed to be a configuration, but has become a bit of a library
-- as well. I should really factor out the two components
module MyPrompts (
    myKeyMap,
    fuzzysearch,
    boolfuzzysearch,
    XPConfig(..),
    XWindowMap,
    myXPConfig,
    mkColor,
    multiPrompt,
    PromptList,
    KeyMap(..),
    WindowPrompt,
    myPrompts
  )
  where

import Colors
import Groups (groupQuery)
import MyGroups
import qualified Data.Map as Map
import ContribMod.GroupNavigation

import Data.Function ((&))
import Data.List (intercalate, sort, sortOn, nub)
import Data.Char (isUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad (replicateM_, forM)
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
import qualified XMonad.StackSet as W

import System.FilePath (takeBaseName)
import XMonad.Util.Run (spawnPipe, hPutStr)
import System.IO (hClose)
import Control.Exception (bracket)


data KeyMap = Action [String] String (X())
            | Motion [String] String (Int -> X())
            | Group  [String] String (X()) [KeyMap]
           -- | Selection [String] String (Int -> X(Window))
           -- | Action [String] String (X(Window) -> Int -> X(Window))
type PromptList = [KeyMap]


-- allWindows is of type X (Map String Window) - so we can change the name
myKeyMap :: String -> XPConfig -> X() -> [KeyMap] -> XConfig l -> 
              M.Map (KeyMask,KeySym) (X())
myKeyMap key xpconf after promptlist xconf =
  mkmap xconf after promptlist key (multiPrompt xpconf promptlist)
  -- $ Action (return key) "" (multiPrompt xpconf promptlist >> after)
  -- : promptlist


-- | Motion Command - accepts a count
--   This would be horrifically inefficient if it weren't for laziness
--   There is still going to be an element of memory leaking here
--   as this is an infinite data structure
mkmap :: XConfig l -> X() -> [KeyMap] -> String -> (Int -> X())
          -> M.Map (KeyMask, KeySym) (X())
mkmap cf after list promptkey prompt =  go 0 ["M-"] list
  where go n mask xs = M.map (>> after) . mkKeymap cf $
                  timesN mask n (Motion (return promptkey) "List Options" prompt)
                  ++ concatMap (timesN mask n) xs ++ do
                     i <- [0..9]
                     m <- mask
                     return ( m ++ show i
                            , submap $ go (i+10*n) (nub ("":mask)) xs)

        c 0 = 1
        c n = n

        timesN mask n (Action keys _ action) =
          allKeys mask keys $ replicateM_ (c n) action
        timesN mask n (Motion keys _ action) = allKeys mask keys $ action (c n)
        timesN mask n (Group  keys _ def subkeys) =
          allKeys mask keys . submapDefault (replicateM_ (c n) def)
                       $ go n (nub ("":mask)) subkeys

        allKeys mask keys action = map (,action) (maybeMask <$> mask <*> keys)


-- | special keys don't need a mask
maybeMask :: String -> String -> String
maybeMask m x@('<':_) = x
maybeMask m x@('M':'-':_) = x
maybeMask m xs = m ++ case xs of
  [a] -> if isUpper a then ['S','-',toLower a] else [a]
  xs' -> xs'

myPrompts :: XPConfig -> XWindowMap -> PromptList
myPrompts conf  windows  = 
        [ Action ["w"] "Go to window" $ do
                      windows <- getallwindows
                      liftIO $ bracket (spawnPipe "menu ! wmctrl -ia")
                        hClose
                        (flip hPutStr (unlines windows))
                      -- windowPrompt winConf Goto windows),
        , Action ["S-w"] "Bring window to master" $ do
                      windows <- getallwindows
                      liftIO $ bracket (spawnPipe "menu ! wmctrl -iR")
                        hClose
                        (flip hPutStr (unlines windows))
        , Action [] "Bring a copy" (
                      windowPrompt winConf BringCopy windows)
        , Action ["M-s"] "Run (sh)" (
                      shellPrompt shellConf)
        , Action ["M-S-s"] "Run (term)" (
                      prompt "urxvt -e" termConf)
        ]
  where shellConf = mkColor nohlconf yellow
        termConf  = mkColor nohlconf red
        winConf   = mkColor conf violet
        runConf   = mkColor conf green
        nohlconf = conf {alwaysHighlight = False}


workspacemap :: X (Map.Map Window String)
workspacemap = withWindowSet $ \ws -> return $
  let workspaces = (map W.workspace (W.current ws : W.visible ws) ++ W.hidden ws)
  in Map.fromList $ flip concatMap workspaces $ \ws ->
      map (,W.tag ws) (fromMaybe [] $ W.integrate <$> W.stack ws)

taking :: Int -> String -> String
taking 0 xs = ""
taking n "" = ' ':taking (n-1) ""
taking n (x:xs) = x:taking (n-1) xs

retaking n x = reverse (taking n (reverse x))

fgcolor :: String -> String -> String
fgcolor a b = "\"$(fgcolor '"++a++"' \""++b++"\")\""

bold :: String -> String
bold a = "\"$(bold \""++a++"\")\""

italic :: String -> String
italic a = "\"$(italic \""++a++"\")\""

a /./ b = a ++ " " ++ b

getallwindows :: X [String]
getallwindows = do
  wsmap <- workspacemap
  windows <- orderedWindowList AllHistory
  forM (toList windows) $ \w -> do
    let workspace = fromMaybe "unknown" $ Map.lookup w wsmap
    group <- groupQuery groups w
    title <- runQuery title w
    return $ fgcolor base1 (retaking 10 (show w))
          /./ fgcolor (accentcolors accentmap workspace) (taking 8 (takeBaseName workspace))
          /./ fgcolor base0 (retaking 8 group)
          /./ bold (fgcolor (accentcolors accentmap group) title)



fuzzysearch :: Double -> String -> String -> Maybe Double
fuzzysearch n [] _ = Just 0
fuzzysearch n (q:qs) (x:xs)
    | toLower x == toLower q = (sqrt n +) <$> fuzzysearch 0 qs xs
    | otherwise              = fuzzysearch (n+1) (q:qs) xs
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


removeMod ('M':'-':xs) = removeMod xs
removeMod (x:xs) = x : removeMod xs
removeMod [] = []

multiPrompt :: XPConfig -> PromptList -> Int -> X ()
multiPrompt conf list n =
  inputPromptWithCompl newconf "" completions ?+ promptFromTitle
  where
    promptMap = list
              & concatMap km
              & map format
              & M.fromList

    km (Action key title prompt) = [ (key, title, prompt) ]
    km (Motion key title prompt ) = [ (key, title ++ "; N ->", prompt n) ]
    km (Group  key title def xs) = (key, title, multiPrompt conf xs n) : x
      where x = [ (liftA2 (++) key k, t, p) | (k,t,p) <- concatMap km xs ]


    format (key, title, prompt) = ( title ++
             if not (null key)
               then " (" ++ removeMod (intercalate ", " (sort key)) ++ ")"
               else ""
           , prompt)

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



