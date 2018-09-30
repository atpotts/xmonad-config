module MyXmobar(myxmobar, launchxmobar) where
import           Groups (groupQuery)
import           MyGroups (groups, accentmap)
import           Colors


import qualified XMonad as X
import           XMonad.Util.Run (spawnPipe)
import           XMonad ((<+>), X, Event, ScreenId(S), runQuery, title, className)
import qualified XMonad.StackSet as W

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Util.Loggers

import System.FilePath (takeBaseName)
import Data.Monoid (All)
import Data.List (isInfixOf)
import Data.Function ((&))
import Data.Char (toUpper)

-- 1. Actual Configuration
--------------------------

myLogTitle :: String -> Logger
myLogTitle bg = X.withWindowSet $ traverse x . W.peek
  where
    x w = do
      thetitle <- runQuery title w
      g <- groupQuery groups w
      let f = xmobarColor (accentcolors accentmap g) bg
      return . f . pad . pad .shorten 90 $ thetitle

currentWorkspace :: String -> Logger
currentWorkspace bg = X.withWindowSet $ \ws -> do
    let w = W.tag . W.workspace . W.current $ ws
    return . return $ xmobarColor bg (accentcolors accentmap w) $ pad $ pad $ takeBaseName w

short = take 3 . takeBaseName

wrapbrack x = "["++x++"]"
plain = pad . pad . short
current = pad . wrapbrack . short
colorize x = xmobarColor (accentcolors accentmap x)

type Color = String
myPP :: Color ->  PP
myPP bg = xmobarPP
    { ppCurrent = \x -> colorize x bg $ current x
    , ppVisible =
        \x -> colorize x base2 $ plain x
    , ppHidden =
        \x -> colorize x bg $ plain x
    , ppHiddenNoWindows = const ""
    , ppLayout = xmobarColor base00 bg . pad . layoutformatter
    , ppTitle = const ""
    , ppExtras = [ currentWorkspace bg, myLogTitle bg ]
    , ppSep = ""
    , ppWsSep = ""
    , ppOrder = \(workspaces:layout:title:ws:rest) -> layout : workspaces : ws : rest
    , ppSort = DO.getSortByOrder
    , ppOutput = ppOutput xmobarPP . xmobarColor base00 bg
    }



isMirror x = isInfixOf "Mirror" x || isInfixOf "Horizontal" x

layoutformatter s
  | "Full" `isInfixOf` s = "      "
  | "TwoPane" `isInfixOf` s =
    if isMirror s
      then "━━━"
      else " ┃ "
  | isMirror s = "━┯━"
  | otherwise = " ┠─"

-- 2. Convenient exports
------------------------

startup :: DynamicStatusBar
startup (S screen) = spawnPipe $
  "xmobar --screen=" ++ show screen ++ " ~/.xmonad/xmobarrc"

cleanup :: DynamicStatusBarCleanup
cleanup =  return () -- X.spawn "killall xmobar"

launchxmobar :: X()
launchxmobar = dynStatusBarStartup startup cleanup

eventhook :: Event -> X All
eventhook = dynStatusBarEventHook startup cleanup

loghook :: X()
loghook = multiPP (myPP base3) (myPP base2)

myxmobar :: X.XConfig a -> X.XConfig a
myxmobar conf = docks conf {
  X.startupHook      = launchxmobar <+> X.startupHook conf,
  X.handleEventHook  = eventhook    <+> X.handleEventHook  conf,
  X.logHook          = loghook      <+> X.logHook    conf,

  X.manageHook       = manageDocks  <+> X.manageHook conf
}

