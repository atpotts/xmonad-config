
module WindowTags where

import XMonad
import XMonad.StackSet as W
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows
import XMonad.Util.EZConfig
import Data.List
import Data.Maybe (catMaybes)

tagSubMap :: [(String, String)]
tagSubMap = do
  letter <- ['a' .. 'z']
  [([letter], "Mark-" ++ [letter]), ("M-" ++ [letter], "Mark-" ++ [letter])]

makemarks c =
  submap $
  mkKeymap c $
  map
    (\(km, s) ->
       ( km
       , do w <- withWindowSet (return . W.peek)
            case w of
              Nothing -> return ()
              Just w' -> do
                withTagged s (delTag s)
                addTag s w'))
    tagSubMap

tomarks c =
  submap $ mkKeymap c $ map (\(km, s) -> (km, focusUpTaggedGlobal s)) tagSubMap

getmarks w = do
  marks <- getTags w
  case catMaybes $ map (stripPrefix "Mark-") marks of
       [] -> return Nothing 
       (x:_):_ -> return (Just x)

-- Tag
withselected = withTagged "*"
clearSelection = withTagged "*" (delTag "*")
addtoselection = do w <- withWindowSet (return . W.peek)
                    sequence_ $ (addTag "*") <$> w
