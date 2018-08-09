module Colors (
base03, base02, base01, base00,
  base0, base1, base2, base3,
  yellow, orange, red, magenta,
  violet, blue, cyan, green, inherit,
  myFont, myBodyFont, accentcolors, addcolor,scale) where

import Data.Hashable (hashWithSalt)
import qualified Data.Map as M
import Colorscheme

scale :: Integral a => a -> a
scale x = round $ (fromIntegral x) * (size/14.0 :: Float)

-- emacs and firefox were the same color with the default salt
accentcolors :: M.Map String String -> String -> String
accentcolors m n =
           case M.lookup n m of
             Just x -> x
             Nothing ->
               if n `elem` map show ([0..9]::[Int])
                  then addcolor $ read n
                  else addcolor $ hashWithSalt 1897 n

addcolor :: Int -> String
addcolor n = case n `mod` 9 of
 1 -> violet
 2 -> blue
 3 -> cyan
 4 -> green
 5 -> yellow
 6 -> orange
 7 -> red
 8 -> magenta
 _ -> base1
 


