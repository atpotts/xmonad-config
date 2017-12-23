module Colors (
base03, base02, base01, base00,
  base0, base1, base2, base3,
  yellow, orange, red, magenta,
  violet, blue, cyan, green, inherit,
  colorscheme, switchcolor, myFont, accentcolors, addcolor) where

import Data.Hashable (hashWithSalt)
import qualified Data.Map as M
import Solarized


myFont :: String
-- myFont = "xft:Meslo LG L DZ:size=10"
myFont = "xft:Inconsolata LGC for Powerline:size=10"
-- myFont = "xft:Liberation Sans:size=10"
-- myFont = "xft:TeX Gyre Adventor:size=10"
-- myFont = "xft:Cabin Regular:size=10"


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
 _ -> base01
 


