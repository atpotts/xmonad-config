module Shrinker where

import ContribMod.Decoration
import Data.Maybe (catMaybes)
import Data.List (unfoldr)

data MyShrinker = MyShrinker [Char] deriving (Read,Show)
instance Shrinker MyShrinker where
  shrinkIt _ "" = []
  shrinkIt (MyShrinker ss) str = str : unfoldr (fmap (\x -> (x, x)) . shrnk) str
    where
      shrnk :: String -> Maybe String
      shrnk [] = Nothing
      shrnk s =
        case catMaybes $ map (\c -> fmap unwords $ dropfirst c $ words s) ss of
          (x:_) -> Just x
          [] ->
            case words s of
              [x] -> Just $ init x
              xs -> Just . unwords $ init xs
      dropfirst c [] = Nothing
      dropfirst c (x:xs)
        | c `elem` x = Just $ tail (dropWhile (/= c) x) : xs
        | otherwise = (x :) <$> dropfirst c xs
