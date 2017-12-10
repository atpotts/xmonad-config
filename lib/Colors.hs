module Colors (
base03, base02, base01, base00,
  base0, base1, base2, base3,
  yellow, orange, red, magenta,
  violet, blue, cyan, green, inherit,
  colorscheme, switchcolor, myFont, accentcolors, addcolor) where

import Data.Hashable (hashWithSalt)
import Solarized


myFont :: String
myFont = "xft:Meslo LG L DZ:size=11"


-- emacs and firefox were the same color with the default salt
accentcolors :: String -> String
accentcolors n | n `elem` map show ([0..9]::[Int]) = addcolor $ read n
               | otherwise = addcolor $ hashWithSalt 107 n

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
 _ -> base01
 


