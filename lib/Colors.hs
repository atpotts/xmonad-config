module Colors where



myFont :: String
myFont = "xft:Meslo LG L DZ:size=11"

base03, base02, base01, base00,
  base0, base1, base2, base3,
  yellow, orange, red, magenta,
  violet, blue, cyan, green, inherit  :: String
base03  ="#002b36"
base02  ="#073642"
base01  ="#586e75"
base00  ="#657b83"
base0   ="#839496"
base1   ="#93a1a1"
base2   ="#eee8d5"
base3   ="#fdf6e3"
yellow  ="#b58900"
orange  ="#cb4b16"
red     ="#dc322f"
magenta ="#d33682"
violet  ="#6c71c4"
blue    ="#268bd2"
cyan    ="#2aa198"
green   ="#859900"
inherit = ""

accentcolors :: String -> String
accentcolors n = case n of
 "1" -> violet
 "2" -> blue
 "3" -> cyan
 "4" -> green
 "5" -> yellow
 "6" -> orange
 "7" -> red
 "8" -> magenta
 _   -> base02
