{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- My Personal Customizations
import           Colors
import           ContribMod.Decoration
  ( DecorationMsg(..)
  , Shrinker(..)
  , SubTheme(..)
  , Theme(..)
  , SubThemeClass
  , DefaultShrinker
  , mkSubTheme
  )


import           ContribMod.TabGroups
import           ContribMod.Tabbed
import           ContribMod.GroupNavigation
import           MyPrompts
import           Projects
import           Shrinker
import           SystemKeys hiding (Toggle)
import           WindowNames
import           WindowTags
import           GroupMotions
import qualified Groups as Grp
import           MyGroups (groups, accentmap, myTerminal)
import           MyXmobar (myxmobar, launchxmobar)

import XMonad
import qualified XMonad.StackSet as W

import           XMonad.Actions.CycleWS
import           XMonad.Actions.SinkAll
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.DynamicWorkspaces (removeEmptyWorkspaceByTag)
import           XMonad.Actions.Submap
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowBringer (bringWindow)
--import           XMonad.Actions.UpdatePointer


import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.Place

import qualified XMonad.Layout.BoringWindows as B
import           XMonad.Layout.Minimize
import           ContribMod.LayoutGroups (GroupsMessage(..))
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Spacing (spacing)
import qualified ContribMod.LayoutGroups as Gr
import qualified ContribMod.LayoutGroupHelpers as G
import           XMonad.Layout.TwoPane
import           XMonad.Hooks.ManageDocks (avoidStruts) -- need to u/s types!

import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.Font
import           XMonad.Util.Image
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run (spawnPipe)


-- Ordinary Haskell Modules
import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Monad (forM_,replicateM_,when,filterM,guard)
import           Control.Applicative (liftA2)

import           Data.Char
import           Data.Function ((&))
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.List (break, isInfixOf, isPrefixOf, isSuffixOf,nub,
                            stripPrefix, unfoldr)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           System.Exit (ExitCode(ExitSuccess), exitWith, exitSuccess)
import           System.IO

activeborder    = base01
backgroundColor = base2
inactiveborder  = base2

border :: Integral a => a
border          = scale 2

data WindowManage = WM {
  onSame :: X(),
  onOther :: X()
}
instance Default WindowManage where def = let r = return () in WM r r


titleOverrides = Grp.titleOverrides Grp.name groups
groupOverrides = Grp.groupOverrides Grp.name groups
groupQuery = Grp.groupQuery groups

flipgroups :: Set.Set String
flipgroups = groups
           & filter Grp.flipCols
           & map Grp.name
           & Set.fromList

myTheme :: Theme MyTheme
myTheme = def {
         activeColor         = winActiveColor defST,
         inactiveColor       = winInactiveColor defST,
         urgentColor         = magenta,
         activeBorderColor   = blue,
         inactiveBorderColor = base00,
         urgentBorderColor   = base00,
         urgentTextColor     = base02,
         activeTextColor     = winActiveTextColor defST,
         inactiveTextColor   = winInactiveTextColor defST,
         fontName            = myFont,
         decoHeight          = scale 26,
         tabBorderWidth      = scale 3
}

data MyTheme = MyTheme deriving (Read, Typeable, Show)
instance Default MyTheme where def = MyTheme
instance SubThemeClass MyTheme where
  mkSubTheme MyTheme = themeWindow border defST


box b n x y =
  let vborder = replicate b True
      hborder = replicate b (replicate (n + 2 * b) True)
      insides i = replicate (n - i) x ++ replicate i y
   in hborder ++ [vborder ++ insides i ++ vborder | i <- [1 .. n]] ++ hborder

ttc :: TiledTabsConfig MyTheme DefaultShrinker
ttc =  (def::TiledTabsConfig MyTheme DefaultShrinker) { tabsTheme = myTheme, tabSpacing = 9 }

myLayout =
  workspaceDir "~" $
  minimize $
  tallTabs 0 $ ttc
      `newTabsShrinker` MyShrinker "/"

-- key overrides
cmdkey = mod3Mask

a /./ b = a <> " " <> b
a /=/ b = a <> " " <> "\"" <> b <> "\""
a /&/ b = a <> " && " <> b

retheme = sendMessage . ToAll . SomeMessage $ SetTheme myTheme

type ColorStr = String

setbg :: ColorStr -> ColorStr -> X ()
setbg fg bg =
  spawn $
  "hsetroot"
                   -- /./ "-bitmap" /=/ home ".xmonad/xbg.xbm"
   /./
  "-solid" /=/
  bg
                   -- /./ "-bg" /=/ bg
                   --
                   --

-- | This is a horrifc abuse of typeclasses to enable
-- the group-level managehooks to be specified late by implicityl
-- threading around information.
--
-- On the other hand, it seems to work quite nicely
--
-- TODO : Refactor this out into something cleaner (probably an additional type)
--  parameter


instance Gr.GroupHook (Gr.Groups a b c) a where
      mHook _ = Grp.groupQuery' Grp.manageHook groups

managementHooks :: [ManageHook]
managementHooks =
  [ resource =? "stalonetray" --> doIgnore
      --liftX G.moveToNewGroupDown
      --liftX (
      --sendMessage Gr.Refocus >> G.moveToNewGroupDown) >> idHook
  ]
  ++ map
    (placeHook (fixed (0.5, 0.5)) <+>)
    [ className =? "Xmessage"  --> doFloat
    , resource  =? "Dialog"    --> doFloat
    , title     =? "<<popup>>" --> doFloat
    ]

main = do
      spawn $ "~/.fehbg"
      xmonad -- $ dynamicProjects projects projectHooks
             $ ewmh
             $ myxmobar
             $ def {
              terminal   = myTerminal,
              manageHook = manageHook def <+> composeAll managementHooks,
              layoutHook = B.boringWindows $ avoidStruts myLayout,
              logHook    = historyHook,
              startupHook = do
                  startupHook def
                  setbg magenta backgroundColor,
                  -- retheme,

              -- use Mod key
              modMask = cmdkey,
              workspaces = ["scratch"],
              keys =  \c -> myKeyMap "M-;" myXPConfig
                      (sendMessage Gr.Refocus >> warper)
                      (  myKeys c
                      ++ myPrompts myXPConfig windowNames
                      ++ projectPrompts (mkColor myXPConfig blue)
                      ) c,
              borderWidth = border,
              normalBorderColor = inactiveborder,
              focusedBorderColor = inactiveborder,
              handleEventHook = chdirEventHook
          }

warper = warpToWindow 0 0



myKeys :: XConfig Layout -> PromptList
myKeys conf =
   [ Action ["e"] "Switch to other screen" $ do
      withOtherScreen W.view
   , Action ["E"] "Swap with other screen" $ do
      withOtherScreen W.greedyView
   , Action ["u"] "Send to other screen" $ withFocused $ \w -> do
      withOtherScreen W.view
      windows $ bringWindow w
   , Action ["U"] "switch to workspace 1" $ withFocused $ \w -> do
      withOtherScreen W.greedyView
      windows $ bringWindow w
   , Action ["S-<Return>"] "Launch terminal" (spawn $ XMonad.terminal conf)
   , Action ["d"] "Close window" (kill >> sendMessage Gr.Refocus)
   , Action ["M-<Return>"] "Next layout" nextOuterLayout
      --media keys
   , Action ["<XF86AudioLowerVolume>"] "Vol-" ( spawn "amixer set Master 4000-")
   , Action ["<XF86AudioRaiseVolume>"] "Vol+" ( spawn "amixer set Master 4000+")
   , Action ["<XF86AudioMute>"] "Mute" (spawn "amixer set Master toggle")
   , Action ["'"] "Go To Mark" (tomarks conf)
   , Action ["m"] "Mark" (makemarks conf)
   , Group  ["o","O"] "Launchers" (return ())
     [ Action ["o"] "Open File" (spawn "menu file")
     , Action ["O"] "Open File" (spawn "cd ~; menu file")
     , Action ["m"] "Open Manpage" (spawn "menu man")
     , Action ["p"] "Open Program" (spawn "menu path")
     ]
   , Action ["C-o"] "Back" $ nextMatch BackwardsHistory (return True)
   , Action ["C-i"] "Back" $ nextMatch ForwardsHistory (return True)
   , Action ["<XF86MonBrightnessUp>","`", "S-~"] "Screen Brightness Up" (spawn "xbacklight -inc +10")
   , Action ["<XF86MonBrightnessDown>", "S-`", "~"] "Screen Brightness Down" (spawn "xbacklight -inc -10")

      -- resizing the master/slave ratio
   , Action ["S-=", "M-="] "Expand master"  expandMasterGroups
   , Action ["-"] "Shrink master" shrinkMasterGroups
      -- floating layer support
   , Action ["t"] "Sink" (withFocused $ windows . W.sink)
   , Action ["T"] "Sink all" sinkAll
      -- increase or decrease number of windows in the master area
   , Action [","] "Increment master" increaseNMasterGroups
   , Action ["."] "Decrement master" decreaseNMasterGroups
      -- quit, or restart
   , Action ["Q", "S-<Backspace>"] "Quit xmonad" (io exitSuccess)
   , Action ["q"] "Restart xmonad" (spawn $
        "if type xmonad;" /./ "then xmonad --recompile && xmonad --restart;" /./
        "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
   , Action ["S-,", "M-<"] "Cycle within app" cycleapp
    ]
    --   was c >> retheme - should no longer be necsessary
    ++ (
      map (\(a,b,c) -> Action a b (c>>warper>>cleanupworkspaces))
       [ ( ["C-<L>", "C-h"]
         , "Send to previous project"
         , DO.shiftTo Prev NonEmptyWS)
       , ( ["C-<R>", "C-l"]
         , "Send to next project"
         , DO.shiftTo Next NonEmptyWS)
       , (["M-<L>", "h"], "Previous project",
           DO.moveTo Prev HiddenNonEmptyWS)
       , (["M-<R>", "l"], "Next project",
           DO.moveTo Next HiddenNonEmptyWS)
       , ( ["M-S-<L>", "H"]
         , "Swap project Left"
         , DO.swapWith Prev NonEmptyWS)
       , ( ["M-S-<R>", "L"]
         , "Swap project Right"
         , DO.swapWith Next NonEmptyWS)
       ])
     ++ systemKeys
     ++

      -- jk/JK - focus up & focus down
      -- *->
      -- move into down/up
      -- move down/up to new
      -- move to master
      -- swap to master
      -- move focus to n
      -- focus down/up

       -- Launch groups
       [ Group ["a"] "Launch Group" cycleapp (
            Action   ["a"] "Cycle" cycleapp
            : flip map groups (\x ->
                Action (Grp.keys x) (Grp.name x) $
                     nextMatchOrDo (BackwardsWhen (Grp.inGroup x))
                        (liftA2 (&&) inWorkSpace (Grp.inGroup x))
                        (spawn $ Grp.spawn x))
            ++ flip map groups (\x ->
                Action (map (map toUpper) $ Grp.keys x) ("Spawn "++ Grp.name x)
                    (spawn $ Grp.spawn x)))

        , Action ["b"] "Breakout Group" (do
            pullToMaster (ask >>= liftX . groupQuery))

        , Action ["B"] "Breakup / Split Group" (do
            sendMessage $ Modify Gr.splitGroup)

        -- Relative Motions - internal
        , Motion ["f"] "Swap with window N" (\n -> modgroups (rotateInGroup n))
        , Motion ["S-c"] "Move to Group N" (\n -> modgroups (moveToGroup n))
        --- F doesn't do anything
        , Motion ["F"] "Focus to Group N" (\n -> modgroups (focusInGroup n))
        , Motion ["S-r"] "Rotate Group to N" (\n -> modgroups (rotateTo n))
        , Motion ["c"] "Move to new group N" (\n ->
            sendMessage (Modify $ moveToNewGroupN n) >>
            sendMessage Gr.Refocus)
                                  -- modgroups (moveToGroup n)
                                  -- G.moveToNewGroupUp
                                  -- sendMessage Gr.Refocus )
        , Motion ["M-<Tab>","n"] "Focus Next" (modgroupsn fNext)
        , Motion ["M-S-<Tab>","p"] "Focus Prev" (modgroupsn fPrev)
        , Motion ["\\","N"] "Swap Next" (modgroupsn sNext)
        , Motion ["S-\\","P"] "Swap Prev" (modgroupsn sPrev)

        -- Relative Motions -- groups
        , Motion ["S-["] "Create Group Above" (
            modgroupsXn1 (G.moveToGroupUp True) $ do
              G.moveToNewGroupUp)
              -- retheme)
        , Motion ["S-]"] "Create Group Below" (
            modgroupsXn1 (G.moveToGroupDown True) $ do
              G.moveToNewGroupDown)
              -- retheme)
        , Motion ["["] "Move to Group Above" (
            modgroupsXn $ G.moveToGroupUp True)
        , Motion ["]"] "Move to Group Below" (
            modgroupsXn $ G.moveToGroupDown True)
        , Motion ["j", "M-<D>"] "Next window" (
            modgroupsXn G.focusGroupDown)
        , Motion ["k", "M-<U>"] "Previous window" (
            modgroupsXn G.focusGroupUp)
        , Motion ["J", "M-S-<D>"] "Swap window next" (
            modgroupsXn G.swapGroupUp)
        , Motion ["K", "M-S-<U>"] "Swap window previous" (
            modgroupsXn G.swapGroupDown)
        , Motion [")","S-0"] "Merge groups down" (
            modgroupsXn (sendMessage $ Modify mergeGroupsDown))
        , Motion ["(","S-9"] "Merge groups up" (
            modgroupsXn (sendMessage $ Modify mergeGroupsUp))
    ]



cycleapp = nextMatchWithThis BackwardsHistory $
  do a <- inWorkSpace
     if a then ask >>= (liftX . groupQuery)
          else return "OtherWS"

inWorkSpace :: Query Bool
inWorkSpace = do
  w <- ask
  ws <- liftX $ gets windowset
  return $ w `elem` allWindowList ws

pullToMaster :: Eq a => Query a -> X()
pullToMaster q = (withWindowSet $ traverse x . W.peek) >> return ()
  where x w = do
          ws <- gets windowset
          let all = windowList ws
          wg <- runQuery q w
          ws' <- filterM (fmap (==wg) . runQuery q) all
          if null ws' then return () else do
                    sendMessage (Modify $ pullout ws')
                    sendMessage Refocus
                    -- retheme


myFocus :: Window -> X ()
myFocus w = focus w >> (sendMessage $ RestoreMinimizedWin w)

withOtherScreen f = do
  otherscreen <- gets (listToMaybe . W.visible . windowset)
  whenJust otherscreen $ windows . f . W.tag . W.workspace

-- myMoveTo :: Int -> W.Stack Window -> W.Stack Window
-- myMoveTo n s@W.Stack{W.up=up,W.down=down} =
--     let (newup,newdown) = splitAt (n-1) $ reverse up ++ down
--     in s{W.up=reverse newup,W.down=newdown}

allWindowList :: WindowSet -> [Window]
allWindowList xs = windowList xs ++ otherWindowList xs

windowList :: WindowSet -> [Window]
windowList =
  W.current >>> listifyWorkspace

listifyWorkspace = W.workspace >>> W.stack >>> fmap W.integrate >>> foldr const []

otherWindowList :: WindowSet -> [Window]
otherWindowList =
  W.visible >>> concatMap (listifyWorkspace)

--home x = "$HOME" ++ x

-- Default (unused) window config
defST = def {
   winActiveColor         = yellow,
   winInactiveColor       = base0,
   winActiveBorderColor   = activeborder,
   winInactiveBorderColor = base3,
   winActiveTextColor     = base3,
   winInactiveTextColor   = backgroundColor}

-- Window Theming
------------------
setWindowBorder' :: (MonadReader XConf m, MonadIO m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

themeWindow bw st w = do
  app <- groupQuery w
  let flipcol = app `elem` flipgroups
      color = accentcolors accentmap app
      revcolor = winActiveTextColor st
      bgcolor = if flipcol then revcolor else color
      fgcolor = if flipcol then color else revcolor
      b = box 0 (scale 5)
      bx m l r = if m then []
        else [(b True False, OffsetLeft bw bw),
                (map reverse $ b l r, OffsetRight bw bw)]
  wf <- withWindowSet (return . W.peek)
  -- focus <-
  --   case wf of
  --     Nothing -> return []
  --     Just w' ->
  --       if w == w'
  --         then return $ bx True False False
  --         else do
  --           app' <- groupQuery w'
  --           return $
  --             if app' == app
  --               then bx False False False
  --               else bx False True False
  mark <- getmarks w
  let markaddon =
        case mark of
          Nothing -> []
          Just a -> [("[" ++ [a] ++ "]", AlignRightOffset (scale 5))]
  isfocus <- (Just w ==) . W.peek <$> gets windowset

  setWindowBorder' (if isfocus then color else winInactiveBorderColor st) w

  return . Just $
    st
      { winInactiveColor     = bgcolor
      , winInactiveBorderColor = backgroundColor
      , winActiveColor       = if isfocus then color else bgcolor
      , winActiveBorderColor = if isfocus then color else bgcolor
      , winActiveTextColor   = if isfocus then revcolor else fgcolor
      , winInactiveTextColor = fgcolor
      -- , winTitleIcons        = focus
      , winTitleAddons       = markaddon
      }



