{-# LANGUAGE
  TypeApplications,
  OverloadedStrings,
  TupleSections
#-}

-- My Personal Customizations
import MyPrompts
import Colors
import WindowNames
import SystemKeys hiding (Toggle)

import qualified DynamicProjects as DP
import Projects

import ContribMod.Decoration (Theme(..),SubTheme(..),DecorationMsg(..),Shrinker(..))
import ContribMod.Tabbed 
import ContribMod.TabGroups

-- XMonad Modules
import qualified Data.Map as Map
import Data.List (unfoldr,stripPrefix)
import Data.Maybe (catMaybes)

import XMonad

import XMonad.Util.Image
import XMonad.Util.Font
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run       (spawnPipe)
import XMonad.Util.EZConfig  (mkKeymap)

import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.TwoPane
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Groups (GroupsMessage(..))

import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.Minimize

import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer

import qualified XMonad.Layout.Groups as Gr 
import qualified XMonad.Layout.Groups.Helpers as Gh

import qualified XMonad.StackSet as W

import XMonad.Actions.Submap
import XMonad.Actions.TagWindows

-- Ordinary Haskell Modules
import Control.Monad (forM)
import Data.Monoid           ((<>))
import Data.Function((&))
import Control.Arrow((>>>),(&&&),(***))
import System.IO
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.List (isInfixOf,break)
import Data.Char
import qualified Data.Map as M


activeborder = base1
inactiveborder = base2
-- Layout Names
-- myLayout = mkToggle (single MIRROR) $ minimize (tall ||| twopane ||| full)
--   where 
--     incstep = 3/100
--     startdivision = 1/2
--     spc = spacing 10 
--     tall    = spc $ Tall 1 incstep startdivision
--     twopane = spc $ TwoPane incstep startdivision
--     full    = spc $ Full
defST = def {
         winActiveColor = yellow,
         winInactiveColor = inactiveborder,
         winActiveBorderColor = activeborder,
         winInactiveBorderColor = base3,
         winActiveTextColor = base3,
         winInactiveTextColor = base02 }

accentmap = M.fromList [
          ("qutebrowser",yellow),
          ("Firefox",yellow),
          ("URxvt",base1),
          ("Gvim",green),
          ("Emacs",green),
          ("scratch",magenta)
          ] 

themeWindow st w = do
      app   <- runQuery className w
      let color = accentcolors accentmap app
          bx l r = [(box (scale 2) (scale 10) l r, OffsetLeft (scale 5) (scale 5)  )]
      wf <- withWindowSet (return . W.peek)
      focus <- case wf of
        Nothing -> return []
        Just w' ->
          if w==w' then return $ bx True True
                   else do
                     app' <- runQuery className w'
                     return $ if app' == app
                              then bx True False
                              else bx False False
      mark <- getmarks w
      let markaddon = case mark of
            Nothing -> []
            Just a -> [("["++[a]++"]",AlignRightOffset (scale 5))]

      -- let focus Nothing = []
      --     focus (Just w') | w == w' = [(diamondbitmap,CenterRight 10)]
      --                     | Just app == appwf = [(emptydiamondbitmap,CenterRight 10)]
                            
      return . Just $ st {
         winInactiveColor = color,
         winActiveColor = color,
         winActiveBorderColor = color,
         winTitleIcons = focus,
         winTitleAddons = markaddon,
         winInactiveTextColor = base2}

myTheme = def {
         activeColor = winActiveColor defST,
         inactiveColor = winInactiveColor defST,
         urgentColor = magenta,
         activeBorderColor = winActiveBorderColor defST,
         inactiveBorderColor = winInactiveBorderColor defST,
         urgentBorderColor = base3,
         urgentTextColor = base02,
         activeTextColor = winActiveTextColor defST,
         inactiveTextColor = winInactiveTextColor defST,
         fontName = myFont,
         decoHeight = (scale 44),

         subThemeForWindow = themeWindow defST
}

box b n x y =
  let   vborder = replicate b True
        hborder = replicate b (replicate (n+2*b) True)
        insides i = replicate (n-i) x ++ replicate (i) y
  in hborder ++ [vborder ++ insides i ++ vborder | i <- [1..n]]  ++ hborder

-- diamondbitmap = map (map (=='+'))
--                [   "       ++       "   ,
--                    "      ++++      "   ,
--                    "     ++++++     "   ,
--                    "    ++++++++    "   ,
--                    "   ++++++++++   "   ,
--                    "  ++++++++++++  "   ,
--                    " ++++++++++++++ "   ,
--                    "++++++++++++++++"   ,
--                    " ++++++++++++++ "   ,
--                    "  ++++++++++++  "   ,
--                    "   ++++++++++   "   ,
--                    "    ++++++++    "   ,
--                    "     ++++++     "   ,
--                    "      ++++      "   ,
--                    "       ++       "   ]

-- emptydiamondbitmap = map (map (=='+'))
--                [   "       ++       "   ,
--                    "      +  +      "   ,
--                    "     +    +     "   ,
--                    "    +      +    "   ,
--                    "   +        +   "   ,
--                    "  +          +  "   ,
--                    " +            + "   ,
--                    "+              +"   ,
--                    " +            + "   ,
--                    "  +          +  "   ,
--                    "   +        +   "   ,
--                    "    +      +    "   ,
--                    "     +    +     "   ,
--                    "      +  +      "   ,
--                    "       ++       "   ]
                   

-- A shrinker for Tab Themes. This Shrinker will first cut word prefixes where appropriate,
-- Then reduce the number of words,
-- And finally reduce the number of characters in the last word.
-- The focus is on beginning by gettign rid of long paths
data MyShrinker = MyShrinker [Char]
instance Show MyShrinker where show _ = ""
instance Read MyShrinker where readsPrec _ s = [(MyShrinker "",s)]        
instance Shrinker MyShrinker where
  shrinkIt _ "" = []
  shrinkIt (MyShrinker ss) str = str : unfoldr (fmap (\x -> (x,x)) . shrnk) str 
    where
      shrnk :: String -> Maybe String
      shrnk [] = Nothing
      shrnk s =
            case catMaybes $ map (\c -> fmap unwords $ dropfirst c $ words s) ss of
                (x:_) -> Just x
                [] -> case words s of
                    [x] -> Just $ init x 
                    xs -> Just . unwords $ init xs
      dropfirst c [] = Nothing
      dropfirst c (x:xs) | c `elem` x = Just $ tail (dropWhile (/=c) x):xs
                         | otherwise  = (x:) <$> dropfirst c xs

myLayout = minimize $ tallTabs $ def {
         tabsTheme = myTheme,
         tabSpacing = 3}
         `newTabsShrinker` MyShrinker "/"
         

isMirror x = isInfixOf "Mirror" x || isInfixOf "Horizontal" x
layoutformatter s
    | "Full" `isInfixOf` s   = "   "
    | "TwoPane" `isInfixOf` s =
        if isMirror s then "━━━" else " ┃ "
    | isMirror s = "━┯━"
    | otherwise = " ┠─"	

-- key overrides
cmdkey = mod3Mask

-- WINDOW TAGS
tagSubMap :: [(String,String)]
tagSubMap = do
      letter <- ['a'..'z']
      [([letter],"Mark-"++[letter]),
       ("M-"++[letter],"Mark-"++[letter])]
makemarks c = submap $ mkKeymap c $
  map (\(km,s) -> (km,do
          w <- withWindowSet (return . W.peek)
          case w of Nothing -> return ()
                    Just w' -> do
                      withTagged s (delTag s)
                      addTag s w')
       ) tagSubMap 
tomarks c = submap $ mkKeymap c $ map (\(km,s) -> (km,focusUpTaggedGlobal s)) tagSubMap

getmarks w = do
  marks <- getTags w
  case catMaybes $ map (stripPrefix "Mark-") marks of
       [] -> return Nothing 
       (x:_):_ -> return (Just x)

--

a /./ b = a <> " " <> b
-- for command line arguments
a /=/ b = a <> " " <> "\"" <> b <> "\""
a /&/ b = a <> " && " <> b

managementHooks :: [ManageHook]
managementHooks = [
    resource =? "stalonetray" --> doIgnore,
    className =? "Xmessage" --> doFloat,
    resource =? "Dialog" --> doFloat
    ]

retheme = sendMessage . ToAll . SomeMessage $ SetTheme myTheme

type ColorStr=String

setbg :: ColorStr -> ColorStr -> X()
setbg fg bg = spawn $ "xsetroot"
                   -- /./ "-bitmap" /=/ home ".xmonad/xbg.xbm"
                   /./ "-solid" /=/ bg
                   -- /./ "-bg" /=/ bg

main = do
      xmproc <- runXmobar
      spawn "offlineimap"
      spawn $ home ".fehbg"
      xmonad $ dynamicProjects projects $ docks defaultConfig {
          -- xmonad $ docks def {
          terminal   = "urxvt -e zsh",
          manageHook = manageDocks <+> manageHook defaultConfig
                                   <+> composeAll managementHooks,
          layoutHook = B.boringWindows $ avoidStruts myLayout,
          logHook    = xmobarHook xmproc,
          startupHook = do
              startupHook defaultConfig
              setbg magenta base2
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
          borderWidth = 2,
          normalBorderColor = inactiveborder,
          focusedBorderColor = activeborder
  }


myKeys :: XConfig Layout -> PromptList
myKeys conf =
    map (\(a,b,c)->(a,b,c>>retheme))$
    [ (["M-S-<Return>"], "Launch terminal", spawn $ XMonad.terminal conf)
    , (["M-d"],       "Close window", kill)
    , (["M-<Return>"],   "Next layout",  nextOuterLayout)
    -- , (["M-\\"], "Toggle Mirror", sendMessage $ Toggle MIRROR)
    , (["M-<Tab>"], "Cycle Group", sendMessage $ Gr.Modify Gr.focusDown)
    , (["M-S-<Tab>"], "Cycle Backwards", sendMessage $ Gr.Modify Gr.focusUp)
    , (["M-\\"], "Move Through Group", 
                  sendMessage (Gr.Modify Gr.swapDown))
    , (["M-S-\\"], "Move Through Group", sendMessage $ Gr.Modify Gr.swapMaster)
    -- , (["M-m"], "Move Through Group", sendMessage $ Gr.Modify Gr.focusMaster)

    , (["M-S-]"], "Create Group Above", sendMessage $ Gr.Modify Gr.moveToNewGroupDown)
    , (["M-S-["], "Create Group Below", sendMessage $ Gr.Modify Gr.moveToNewGroupUp)
    , (["M-["],"Move To PreviousGroup", sendMessage $ Gr.Modify $ Gr.moveToGroupUp True)
      , (["M-]"],"Move to Next Group", sendMessage $ Gr.Modify $ Gr.moveToGroupDown True)

    , (["M-n"], "Reset window size", refresh)

    -- move focus up or down the window stack
    -- , (["M-j","M-<D>","M-<Tab>"],  "Next window", B.focusDown)
    -- , (["M-k","M-<U>","M-S-<Tab>"],"Previous window",B.focusUp  )
    -- , (["M-m"],                     "Go to master", B.focusMaster)
    , (["M-j","M-<D>"],  "Next window", sendMessage $ Gr.Modify Gr.focusGroupDown)
    , (["M-k","M-<U>"],"Previous window",sendMessage $ Gr.Modify Gr.focusGroupUp  )
    , (["M-e"], "Go to master", sendMessage $ Gr.Modify Gr.focusGroupMaster)

    --media keys
    , (["<XF86AudioLowerVolume>","<F11>"],"Vol-", spawn "amixer set Master 4000-")
    , (["<XF86AudioRaiseVolume>","<F12>"], "Vol+", spawn "amixer set Master 4000+")
    , (["<XF86AudioMute>","<F10>"], "Mute", spawn "amixer set Master toggle")
    , (["M-'"],"Go To Mark", tomarks conf)
    , (["M-m"],"Mark",makemarks conf)
    -- , (["<XF86KbdBrightnessUp>"],"Keyboard Brightness Up",
    --     spawn $  home ".nix-profile/bin/kbdlight" /./ "up")
    -- , (["<XF86KbdBrightnessDown>"],"Keyboard Brightness Down",
    --     spawn $  home ".nix-profile/bin/kbdlight" /./ "down")

    , (["<XF86MonBrightnessUp>","<F2>"],"Screen Brightness Up",
         spawn "xbacklight -inc +10")
    , (["<XF86MonBrightnessDown>","<F1>"],"Screen Brightness Down", spawn "xbacklight -inc -10")
    
    -- modifying the window order
    -- , (["M-S-m"],           "Swap with master window", windows W.swapMaster)
    -- , (["M-S-j","M-S-<D>"], "Swap window next",        windows W.swapDown  )
    -- , (["M-S-k","M-S-<U>"], "Swap window previous",    windows W.swapUp    )
    , (["M-S-e"],           "Swap with master group",
                sendMessage $ Gr.Modify Gr.swapGroupMaster)
    , (["M-S-j","M-S-<D>"], "Swap window next",
                sendMessage $ Gr.Modify Gr.swapGroupUp)
    , (["M-S-k","M-S-<U>"], "Swap window previous",
                sendMessage $ Gr.Modify Gr.swapGroupDown)

    -- resizing the master/slave ratio
    , (["M-S-=","M-="], "Expand master",    expandMasterGroups)
    , (["M--"],         "Shrink master",    shrinkMasterGroups)

    -- floating layer support
    , (["M-t"],"Unfloat",          withFocused $ windows . W.sink)

    -- increase or decrease number of windows in the master area
    , (["M-,"],"Increment master", increaseNMasterGroups )
    , (["M-."],"Decrement master", decreaseNMasterGroups)

    -- quit, or restart
    , (["M-S-<Backspace>"], "Quit xmonad", io (exitWith ExitSuccess))
    , (["M-q"], "Restart xmonad",
        spawn $ "if type xmonad;"
              /./"then xmonad --recompile && xmonad --restart;"
              /./"else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

    -- , (["M-S-q"], "Restart xmonad with a new colorscheme",
    --     let lib x = home (".xmonad/lib/"++x)
    --     in spawn $  "rm" /./ lib "Solarized.hs"
    --           /&/ "ln -s" /./ lib switchcolor /./ lib "Solarized.hs"
    --           /&/"if type xmonad;"
    --                  /./"then xmonad --recompile && xmonad --restart;"
    --                  /./"else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- Projects
    , (["M-C-<L>","M-C-h"], "Send to previous project", 
        DO.shiftTo  Prev HiddenNonEmptyWS)
    , (["M-C-<R>","M-C-l"], "Send to next project",     
        DO.shiftTo  Next HiddenNonEmptyWS)
    , (["M-<L>","M-h"],     "Previous project",         
        DO.moveTo   Prev HiddenNonEmptyWS)
    , (["M-<R>","M-l"],     "Next project",             
        DO.moveTo   Next HiddenNonEmptyWS)
    , (["M-S-<L>","M-S-h"], "Swap project Left",        
        DO.swapWith Prev HiddenNonEmptyWS)
    , (["M-S-<R>","M-S-l"], "Swap project Right",       
        DO.swapWith Next HiddenNonEmptyWS)

    , (["M-b"],   "Minimize",   withFocused minimizeWindow)
    , (["M-S-b"], "Unminimize", sendMessage RestoreNextMinimizedWin)
    , (["M-S-,","M-<"], "Cycle within app", do
        ws <- gets windowset
        let p w1 w2 = do
                w1g <- runQuery className w1
                w2g <- runQuery className w2
                return (w1g == w2g)
        case W.stack $ W.workspace $ W.current ws of
             Nothing -> return ()
             Just x -> focusNext p x
        )
    ]
    ++
    systemKeys
    -- ++
    -- [(["M-"++show i],"Show window "++show i,
    --   do ws <- windowList <$> gets windowset
    --      let w = take 1 $ drop (i-1) ws
    --      case w of
    --        [wn] -> myFocus wn
    --        _    -> return ()
    --  )| i <- [1..9]]
    --  ++
    -- [(["M-S-"++show i],"Move window to "++show i,
    --                 windows $ W.modify' (myMoveTo i)
    --  )| i <- [1..9]]

myFocus :: Window -> X ()
myFocus w = focus w >> (sendMessage $ RestoreMinimizedWin w)

focusNext :: (Window -> Window -> X (Bool)) -> W.Stack Window -> X ()
focusNext p s@W.Stack{W.up=up, W.down=down, W.focus=fs} = do
    applist <- sequence $ map (\x -> (,x) <$> p fs x) (down ++ reverse up)
    case map snd $ filter (fst) applist of
         []    -> return () 
         (x:_) -> myFocus  x

runXmobar = spawnPipe $  home ".nix-profile/bin/xmobar"
                     /./ home (".xmonad/xmobarrc")


-- myMoveTo :: Int -> W.Stack Window -> W.Stack Window
-- myMoveTo n s@W.Stack{W.up=up,W.down=down} =
--     let (newup,newdown) = splitAt (n-1) $ reverse up ++ down
--     in s{W.up=reverse newup,W.down=newdown}


windowList :: WindowSet -> [Window]
windowList = W.current 
         >>> W.workspace
         >>> W.stack
         >>> fmap W.integrate 
         >>> foldr const []



myLogTitle :: Logger
myLogTitle = withWindowSet
           $ traverse (anyWindowNamer Nothing
                         (\a -> xmobarColor (accentcolors accentmap a) inherit a) id)
           . W.peek


xmobarHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput  = hPutStrLn xmproc
  , ppCurrent = \n ->
        xmobarColor base3 (accentcolors accentmap n) $ pad $ pad n
  , ppVisible = \x ->
        xmobarColor base0 inherit $ "["<> x <>"]"
  , ppHidden  = \x -> take 1 x
                   & pad
                   & xmobarColor base2 (accentcolors accentmap x)
  , ppHiddenNoWindows = const ""
  , ppLayout = xmobarColor base2 base1 . layoutformatter
  , ppTitle = const ""
  , ppExtras = [
        fmap (fmap (xmobarColor base0 base2 . shorten 90)) myLogTitle
        ]
  , ppSep = " "
  , ppWsSep = ""
  , ppOrder = \(layout : workspaces : title : rest) ->
                workspaces : layout : rest
  , ppSort = DO.getSortByOrder
  }

home x = "/home/alistairtpotts/" ++ x

