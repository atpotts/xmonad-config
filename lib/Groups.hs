{-# LANGUAGE TupleSections #-}

module Groups where

import Control.Arrow ((&&&))
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
  }

titleOverrides = map (title &&& name)

groupOverrides groups =
  Map.fromList $ concatMap (\x -> (, name x) <$> group x) groups

groupQuery :: [GroupDefinition] -> Window -> X String
groupQuery groups w = do
  tit <- runQuery X.title w
  let titles =
        map
          (\(f, v) ->
             if f tit
               then Just v
               else Nothing)
          $ titleOverrides groups
  case catMaybes titles of
    [] -> do
      cls <- runQuery className w
      return $ fromMaybe cls (Map.lookup cls $ groupOverrides groups)
    x:_ -> return x
