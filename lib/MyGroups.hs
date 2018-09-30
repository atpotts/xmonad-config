
module MyGroups(groups, accentmap, myTerminal) where

import Colors
import Groups (GroupDefinition(GD))
import qualified Groups as Grp
import qualified Data.Map as Map

import Data.Default (def)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified ContribMod.LayoutGroups as Gr

import Control.Arrow ((&&&))

termcmd         = "urxvtc"
myTerminal      = termcmd ++ " -e zsh"
launchinterm x  = ((termcmd ++ " --name "++x++" -e ")++)

accentmap :: Map.Map String String
accentmap =
  Map.fromList $
    ("scratch", magenta) : map (Grp.name &&& Grp.colour) groups

groups :: [GroupDefinition]
groups = [
    def { Grp.name="Shell"
        , Grp.keys=["t"]
        , Grp.spawn=myTerminal
        , Grp.colour=base1
        , Grp.title=(isPrefixOf "zsh:")
        , Grp.manageHook=Gr.moveToNewGroupDown
        , Grp.flipCols=True
        }
  , def { Grp.name="Terminal"
        , Grp.keys=["T"]
        , Grp.spawn=myTerminal
        , Grp.colour=blue
        , Grp.group=["URxvt", "Termite"]
        , Grp.manageHook=Gr.moveToNewGroupDown
        , Grp.flipCols=True
        }
  , def { Grp.name="Terminal Keep"
        , Grp.colour=base1
        , Grp.title=(=="<<keep>>")
        , Grp.flipCols=True
        }
  , def { Grp.name="Editor"
        , Grp.keys=["e"]
        , Grp.spawn=launchinterm "Kakoune" "kak"
        , Grp.colour=green
        , Grp.title= \x -> "Kakoune" `isSuffixOf` x || "VIM" `isSuffixOf` x
                          || ("**" `isPrefixOf` x && "EDITOR" `isInfixOf` x)
        , Grp.group=["Emacs", "Kakoune"]
        , Grp.flipCols=True
        }
  , def
        { Grp.name = "Browser"
        , Grp.keys=["b"]
        , Grp.spawn="qutebrowser"
        , Grp.colour=yellow
        , Grp.title=const False
        , Grp.group=["qutebrowser", "Firefox", "Chromium"]
        , Grp.flipCols=True
        }
  ]

