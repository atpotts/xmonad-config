{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
-- This is
-- A secnod line and I think that it will be much better if I allow it to run on
-- and on and on.

-- My Personal Customizations
import           Colors
import           ContribMod.Decoration
  ( DecorationMsg(..)
  , Shrinker(..)
  , SubTheme(..)
  , Theme(..)
  )
import           ContribMod.TabGroups
import           ContribMod.Tabbed
import qualified DynamicProjects as DP
import           MyPrompts
import           Projects
import           SystemKeys hiding (Toggle)
import           WindowNames
import           WindowTags


import XMonad
import qualified XMonad.StackSet as W

import           GroupMotions

import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.Submap
import           XMonad.Actions.UpdatePointer

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Place

import qualified XMonad.Layout.BoringWindows as B
import           XMonad.Layout.Minimize
import           XMonad.Layout.Groups (GroupsMessage(..))
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Spacing (spacing)
import qualified XMonad.Layout.Groups as Gr
import qualified XMonad.Layout.Groups.Helpers as G
import           XMonad.Layout.TwoPane

import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.Font
import           XMonad.Util.Image
import           XMonad.Util.Loggers
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run (spawnPipe)


import           Control.Arrow ((&&&), (***), (>>>))

-- Ordinary Haskell Modules
import           Control.Monad (forM,replicateM_)
import           Control.Applicative (liftA2)

import           Data.Char
import           Data.Function ((&))
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid ((<>))
import           Data.List (break, isInfixOf, isPrefixOf, isSuffixOf,nub,
                            stripPrefix, unfoldr)
import qualified Data.Map as Map

import           System.Exit (ExitCode(ExitSuccess), exitWith)
import           System.IO

activeborder = base01
backgroundColor = base2
inactiveborder = backgroundColor

defST = def {
         winActiveColor         = yellow,
         winInactiveColor       = base0,
         winActiveBorderColor   = activeborder,
         winInactiveBorderColor = backgroundColor,
         winActiveTextColor     = base3,
         winInactiveTextColor   = backgroundColor}
data GroupDefinition = GD
  { groupName   :: String
  , groupKeys   :: [String]
  , groupSpawn  :: String
  , groupColour :: String
  , groupTitle  :: String -> Bool
  , groupGroup  :: [String]
  }

groups :: [GroupDefinition]
groups =
  [ GD "Shell"    ["M-a"]   "urxvt" base00 (isPrefixOf "zsh:") []
  , GD "Terminal" ["M-S-a"] "urxvt" blue   (const False) ["URxvt", "Termite"]
  , GD 
      "Editor"
      ["M-S-b"]
      "urxvt -e kak"
      green
      (\x -> "Kakoune" `isSuffixOf` x || "VIM" `isSuffixOf` x)
      ["Emacs"]
  , GD
      "Browser"
      ["M-b"]
      "qutebrowser"
      yellow
      (const False)
      ["qutebrowser", "Firefox", "Chromium"]
  ]

accentmap :: Map.Map String String
accentmap =
  Map.fromList $
  [ ("scratch", magenta) ] ++
  map (groupName &&& groupColour) groups

titleOverrides = map (groupTitle &&& groupName) groups

groupOverrides =
  Map.fromList $ concatMap (\x -> (, groupName x) <$> groupGroup x) groups


groupQuery :: Window -> X String
groupQuery w = do
  tit <- runQuery title w
  let titles =
        map
          (\(f, v) ->
             if f tit
               then Just v
               else Nothing)
          titleOverrides
  case catMaybes titles of
    [] -> do
      cls <- runQuery className w
      return $ fromMaybe cls (Map.lookup cls groupOverrides)
    x:_ -> return x

setWindowBorder' ::(MonadReader XConf m, MonadIO m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

themeWindow bw st w = do
  app <- groupQuery w
  let color = accentcolors accentmap app
      b l r = box 0 (scale 10) l r
      bx m l r = if m then []
        else [(b True False, OffsetLeft bw bw),
                (map (reverse) $ b l r, OffsetRight bw bw)]
  wf <- withWindowSet (return . W.peek)
  focus <-
    case wf of
      Nothing -> return []
      Just w' ->
        if w == w'
          then return $ bx True False False
          else do
            app' <- groupQuery w'
            return $
              if app' == app
                then bx False False False
                else bx False True False
  mark <- getmarks w
  let markaddon =
        case mark of
          Nothing -> []
          Just a -> [("[" ++ [a] ++ "]", AlignRightOffset (scale 5))]
  isfocus <- (Just w ==) . W.peek <$> gets windowset
  if isfocus then setWindowBorder' color w else return ()
  return . Just $
    st
      { winInactiveColor     = color
      , winActiveColor       = color
      , winActiveBorderColor = color
      , winTitleIcons        = focus
      , winTitleAddons       = markaddon
      }


myTheme = def {
         activeColor         = winActiveColor defST,
         inactiveColor       = winInactiveColor defST,
         urgentColor         = magenta,
         activeBorderColor   = winActiveBorderColor defST,
         inactiveBorderColor = winInactiveBorderColor defST,
         urgentBorderColor   = base3,
         urgentTextColor     = base02,
         activeTextColor     = winActiveTextColor defST,
         inactiveTextColor   = winInactiveTextColor defST,
         fontName            = myFont,
         decoHeight          = (scale 46),
         tabBorderWidth      = scale 4,
         subThemeForWindow   = themeWindow (scale 4) defST
}

box b n x y =
  let vborder = replicate b True
      hborder = replicate b (replicate (n + 2 * b) True)
      insides i = replicate (n - i) x ++ replicate (i) y
   in hborder ++ [vborder ++ insides i ++ vborder | i <- [1 .. n]] ++ hborder


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

myLayout =
  minimize $
  tallTabs (scale 4) $
  def { tabsTheme = myTheme, tabSpacing = 10 } `newTabsShrinker` MyShrinker "/"

isMirror x = isInfixOf "Mirror" x || isInfixOf "Horizontal" x

layoutformatter s
  | "Full" `isInfixOf` s = "      "
  | "TwoPane" `isInfixOf` s =
    if isMirror s
      then "━━━"
      else " ┃ "
  | isMirror s = "━┯━"
  | otherwise = " ┠─"

-- key overrides
cmdkey = mod3Mask

motion :: XConfig l ->  [([String],String,Int -> X())] -> [([String],String,X())]
motion cf xs = go 0 ["M-"] xs
  where go n mask xs = map (\(a,b,c) -> (liftA2 (++) mask a,b,
                                  c (if n==0 then 1 else n))) xs
                  ++  [( map (++show i) mask
                       ,"Prefix "++show i
                       , submap . (mkKeymap cf) .
                          concatMap (\(a,_ ,c) -> map (,c) a) $
                            go (i+10*n) (nub ("":mask)) xs
                        ) | i <- [0..9] ]

a /./ b = a <> " " <> b

-- for command line arguments
a /=/ b = a <> " " <> "\"" <> b <> "\""

a /&/ b = a <> " && " <> b

managementHooks :: [ManageHook]
managementHooks =
  (resource =? "stalonetray" --> doIgnore) :
  map
    (placeHook (fixed (0.5, 0.5)) <+>)
    [ className =? "Xmessage" --> doFloat
    , resource =? "Dialog" --> doFloat
    , title =? "**popup**" --> doFloat
    ]

retheme = sendMessage . ToAll . SomeMessage $ SetTheme myTheme

type ColorStr=String

setbg :: ColorStr -> ColorStr -> X ()
setbg fg bg =
  spawn $
  "hsetroot"
                   -- /./ "-bitmap" /=/ home ".xmonad/xbg.xbm"
   /./
  "-solid" /=/
  bg
                   -- /./ "-bg" /=/ bg

termcmd = "urxvt"

maybemod :: String -> String
maybemod x@('<':_) = x
maybemod x@('M':'-':_) = x
maybemod xs = "M-"++ case xs of
  [a] -> if isUpper a then ['S','-',toLower a] else [a]
  xs' -> xs'

expand :: [String] -> [String]
expand xs = do
  x <- xs
  let q:qs = words x
  qs' <- sequence (map (\x -> [maybemod x,x]) qs)
  return . unwords $ maybemod q : qs'

main = do
      xmproc <- runXmobar
      spawn "offlineimap"
      spawn $ home ".fehbg"
      xmonad $ dynamicProjects projects projectHooks
             $ ewmh
             $ docks def {
              terminal   = termcmd /./ "-e zsh",
              manageHook = manageDocks <+> manageHook defaultConfig
                                       <+> composeAll managementHooks,
              layoutHook = B.boringWindows $ avoidStruts myLayout,
              logHook    = xmobarHook xmproc,
              startupHook = do
                  startupHook defaultConfig
                  setbg magenta backgroundColor
                  retheme,

              -- use Mod key
              modMask = cmdkey,
              workspaces = ["scratch"],
            keys = \conf -> mkKeymap conf $
                  myKeyMap "M-;" myXPConfig
                        (myKeys conf
                        ++ myPrompts myXPConfig windowNames
                        ++ projectPrompts (mkColor myXPConfig blue)),

            -- Borders
            -- ker for Tab Themes. This Shri\n -> modgroups (replicateM_ n
            -- fNext))
-- Then reduce the number of words,inally reduce the number of characters in the last word.
-- The focus is on beginning by gettign rid of long paths
--
              borderWidth = scale 4,
              normalBorderColor = inactiveborder,
              focusedBorderColor = inactiveborder
          }

myKeys :: XConfig Layout -> PromptList
myKeys conf =
  map (\(a, b, c) -> (expand a, b, c >> sendMessage Gr.Refocus)) ( -- c >> retheme
   [  (["S-<Return>"], "Launch terminal", spawn $ XMonad.terminal conf)
   , (["d"], "Close window", kill >> sendMessage Gr.Refocus)
   , (["M-<Return>"], "Next layout", nextOuterLayout)
      -- , (["M-\\"], "Toggle Mirror", sendMessage $ Toggle MIRROR)

      -- , (["M-m"], "Move Through Group", sendMessage $ Gr.Modify Gr.focusMaster)

      --media keys
   , ( ["<XF86AudioLowerVolume>", "<F11>"]
     , "Vol-"
     , spawn "amixer set Master 4000-")
   , ( ["<XF86AudioRaiseVolume>", "<F12>"]
     , "Vol+"
     , spawn "amixer set Master 4000+")
   , (["<XF86AudioMute>", "<F10>"], "Mute", spawn "amixer set Master toggle")
   , (["'"], "Go To Mark", tomarks conf)
   , (["m"], "Mark", makemarks conf)
   , (["o"], "Open File", spawn "stouter")
      -- , (["<XF86KbdBrightnessUp>"],"Keyboard Brightness Up",
      --     spawn $  home ".nix-profile/bin/kbdlight" /./ "up")
      -- , (["<XF86KbdBrightnessDown>"],"Keyboard Brightness Down",
      --     spawn $  home ".nix-profile/bin/kbdlight" /./ "down")
   , ( ["<XF86MonBrightnessUp>", "<F2>"]
     , "Screen Brightness Up"
     , spawn "xbacklight -inc +10")
   , ( ["<XF86MonBrightnessDown>", "<F1>"]
     , "Screen Brightness Down"
     , spawn "xbacklight -inc -10")
      -- modifying the window order
      -- , (["M-S-m"],           "Swap with master window", windows W.swapMaster)
      -- , (["M-S-j","M-S-<D>"], "Swap window next",        windows W.swapDown  )
      -- , (["M-S-k","M-S-<U>"], "Swap window previous",    windows W.swapUp    )
   , ( ["E"]
     , "Swap with master group"
     , G.swapGroupMaster)
   , ( ["J", "M-S-<D>"]
     , "Swap window next"
     , G.swapGroupUp)
   , ( ["K", "M-S-<U>"]
     , "Swap window previous"
     , G.swapGroupDown)
      -- resizing the master/slave ratio
   , (["S-=", "M-="], "Expand master", expandMasterGroups)
   , (["-"], "Shrink master", shrinkMasterGroups)
      -- floating layer support
   , (["t"], "Unfloat", withFocused $ windows . W.sink)
      -- increase or decrease number of windows in the master area
   , ([","], "Increment master", increaseNMasterGroups)
   , (["."], "Decrement master", decreaseNMasterGroups)
      -- quit, or restart
   , (["S-<Backspace>"], "Quit xmonad", io (exitWith ExitSuccess))
   , ( ["q"]
     , "Restart xmonad"
     , spawn $
        "if type xmonad;" /./ "then xmonad --recompile && xmonad --restart;" /./
        "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
   , ( ["C-<L>", "C-h"]
     , "Send to previous project"
     , DO.shiftTo Prev HiddenNonEmptyWS)
   , ( ["C-<R>", "C-l"]
     , "Send to next project"
     , DO.shiftTo Next HiddenNonEmptyWS)
   , (["M-<L>", "h"], "Previous project",
       DO.moveTo Prev HiddenNonEmptyWS)
   , (["M-<R>", "l"], "Next project",
       DO.moveTo Next HiddenNonEmptyWS)
   , ( ["M-S-<L>", "H"]
     , "Swap project Left"
     , DO.swapWith Prev HiddenNonEmptyWS)
   , ( ["M-S-<R>", "L"]
     , "Swap project Right"
     , DO.swapWith Next HiddenNonEmptyWS)
   , ( ["S-,", "M-<"]
     , "Cycle within app"
     , do ws <- gets windowset
          let p w1 w2 = do
                w1g <- groupQuery w1
                w2g <- groupQuery w2
                return (w1g == w2g)
          case W.stack $ W.workspace $ W.current ws of
            Nothing -> return ()
            Just x -> focusNext (return ()) p x)
    ]
    -- Launch groups
      ++ ( map (\x ->
          ( groupKeys x
          , groupName x
          , do ws <- gets windowset
               let p _ w1 = do
                    w1g <- groupQuery w1
                    return (w1g == groupName x)
               case W.stack $ W.workspace $ W.current ws of
                 Nothing -> return ()
                 Just y -> focusNext (spawn $ groupSpawn x ) p y)
            ) groups ))

     ++ systemKeys
     ++ motion conf [
        -- Absolute Motions
          (["r"],  "Swap to Group N",     \n -> modgroups $ rotateInGroup n)
        , (["S-r"],"Move to Group N",     \n -> modgroups $ moveToGroup n)
        , (["c"],  "Move Group to N",     \n -> modgroups $ rotateTo n)
        , (["S-c"],"Move to new group N", \n -> do
                                  modgroups (moveToGroup n)
                                  G.moveToNewGroupUp
                                  sendMessage Gr.Refocus )
        , (["f"],  "Focus to Group N",    \n -> modgroups $ focusInGroup n)
        , (["S-f"],"Focus to Group N",    \n -> modgroups $ focusN n)

        -- Relative Motions - internal
        , (["<Tab>"]  , "Focus Next", \n ->  modgroupsn n fNext)
        , (["S-<Tab>"], "Focus Prev", \n ->  modgroupsn n fPrev)
        , (["\\"]     , "Swap Next" , \n ->  modgroupsn n sNext)
        , (["S-\\"]   , "Swap Prev" , \n ->  modgroupsn n sPrev)

        -- Relative Motions -- groups
        --
        , ( ["S-["], "Create Group Above", \n -> (
            modgroupsXn1 n (G.moveToGroupUp True) G.moveToNewGroupUp))
        , ( ["S-]"], "Create Group Below", \n -> (
            modgroupsXn1 n (G.moveToGroupDown True) G.moveToNewGroupDown))
        , ( ["["], "Move to Group Above",  \n -> (
            modgroupsXn n $ G.moveToGroupUp True))
        , ( ["]"], "Move to Group Below",  \n -> (
            modgroupsXn n $ G.moveToGroupDown True))
        , (["j", "M-<D>"], "Next window", \n -> (
            modgroupsXn n $ G.focusGroupDown))
        , (["k", "M-<U>"], "Previous window", \n -> (
            modgroupsXn n $ G.focusGroupUp))
     ]

myFocus :: Window -> X ()
myFocus w = focus w >> (sendMessage $ RestoreMinimizedWin w)

focusNext :: X () -> (Window -> Window -> X (Bool)) -> W.Stack Window -> X ()
focusNext def p s@(W.Stack fs up down) = do
  applist <- sequence $ map (\x -> (, x) <$> p fs x) (down ++ reverse up)
  case map snd $ filter (fst) applist of
    [] -> def
    (x:_) -> myFocus x

runXmobar =
  spawnPipe $ home ".nix-profile/bin/xmobar" /./ home (".xmonad/xmobarrc")

-- myMoveTo :: Int -> W.Stack Window -> W.Stack Window
-- myMoveTo n s@W.Stack{W.up=up,W.down=down} =
--     let (newup,newdown) = splitAt (n-1) $ reverse up ++ down
--     in s{W.up=reverse newup,W.down=newdown}


windowList :: WindowSet -> [Window]
windowList =
  W.current >>> W.workspace >>> W.stack >>> fmap W.integrate >>> foldr const []

myLogTitle :: Logger
myLogTitle = withWindowSet $ traverse x . W.peek
  where
    x w = do
      app <- runQuery className w
      thetitle <- runQuery title w
      g <- groupQuery w
      return $
        xmobarColor (accentcolors accentmap g) inherit app ++ ": " ++ thetitle

xmobarHook xmproc =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = \n ->
          xmobarColor base3 (accentcolors accentmap n) $ pad $ pad n
      , ppVisible = \x ->
          xmobarColor base0 inherit $ "[" <> x <> "]"
      , ppHidden = \x ->
          take 1 x & pad & xmobarColor base2 (accentcolors accentmap x)
      , ppHiddenNoWindows = const ""
      , ppLayout = xmobarColor base2 base1 . layoutformatter
      , ppTitle = const ""
      , ppExtras =
          [fmap (fmap (xmobarColor base0 base2 . shorten 90)) myLogTitle]
      , ppSep = " "
      , ppWsSep = ""
      , ppOrder = \(layout:workspaces:title:rest) -> workspaces : layout : rest
      , ppSort = DO.getSortByOrder
      }

home x = "/home/alistairtpotts/" ++ x

