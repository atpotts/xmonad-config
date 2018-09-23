{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Groups where

import Control.Arrow ((&&&))
import Data.Default
import ContribMod.LayoutGroups (ModifySpec)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import XMonad hiding (title)
import qualified XMonad as X

data GroupDefinition = GD
  { name :: String
  , keys :: [String]
  , spawn :: String
  , colour :: String
  , title :: String -> Bool
  , group :: [String]
  , manageHook :: ModifySpec
  , flipCols :: Bool
  }

instance Default GroupDefinition where
  def = GD "" [] "" "#FFFFFF" (const False) ["BLANK"] (flip const) False

titleOverrides f = map (title &&& f)

groupOverrides :: (GroupDefinition -> a) -> [GroupDefinition] -> Map.Map String a
groupOverrides f groups =
  Map.fromList $ concatMap (\x -> (, f x) <$> group x) groups

groupQuery :: [GroupDefinition] -> Window -> X String
groupQuery gs w = do
  x <- runQuery (groupQuery' name gs) w
  case x of
      "BLANK" -> runQuery className w
      _ -> return x

groupQuery' :: (GroupDefinition -> a) -> [GroupDefinition] -> Query a
groupQuery' f groups = do
  w <- ask
  tit <- X.title
  name <- X.appName
  let titles =
        map
          (\(f, v) ->
             if f tit || f name
               then Just v
               else Nothing)
          $ titleOverrides f groups
  case catMaybes titles of
    [] -> do
      cls <- className
      return $ fromMaybe (f def) (Map.lookup cls $ groupOverrides f groups)
    x:_ -> return x

inGroup :: GroupDefinition -> Query Bool
inGroup gd = do
  cls <-  className
  if cls `elem` group gd
    then return True
    else do
      tit <- X.title
      return $ (title gd) tit

