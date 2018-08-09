{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups.Examples
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Example layouts for "XMonad.Layout.Groups".
--
-----------------------------------------------------------------------------

module ContribMod.TabGroups where

import XMonad hiding ((|||))

import ContribMod.Tabbed
import ContribMod.Decoration

import qualified ContribMod.LayoutGroups as G
import ContribMod.LayoutGroupHelpers

import XMonad.Layout.Spacing (spacing)
--import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Simplest


-- $usage
-- This module contains example 'G.Groups'-based layouts.
-- You can either import this module directly, or look at its source
-- for ideas of how "XMonad.Layout.Groups" may be used.
--
-- You can use the contents of this module by adding
--
-- > import XMonad.Layout.Groups.Examples
--
-- to the top of your @.\/.xmonad\/xmonad.hs@.
--
-- For more information on using any of the layouts, jump directly
--   to its \"Example\" section.
--
-- Whichever layout you choose to use, you will probably want to be
--   able to move focus and windows between groups in a consistent
--   manner. For this, you should take a look at the functions from
--   the "XMonad.Layout.Groups.Helpers" module, which are all
--   re-exported by this module.
--
-- For more information on how to extend your layour hook and key bindings, see
--   "XMonad.Doc.Extending".


-- | Configuration data for the "tiled tab groups" layout
data TiledTabsConfig t s = TTC { vNMaster :: Int
                             , vRatio :: Rational
                             , vIncrement :: Rational
                             , hNMaster :: Int
                             , hRatio :: Rational
                             , hIncrement :: Rational
                             , tabsShrinker :: s
                             , tabsTheme :: Theme t
                             , tabSpacing :: Int}

newTabsShrinker :: SubThemeClass t => TiledTabsConfig t s -> y -> TiledTabsConfig t y
newTabsShrinker (TTC vn vr vi hn hr hi _ tt ts) sh =
                TTC vn vr vi hn hr hi sh tt ts

instance (s ~ DefaultShrinker, Default t)
          => Default (TiledTabsConfig t s) where
    def = TTC 1 0.5 (3/100) 1 0.5 (3/100) shrinkText def 0


tallTabs n c = tiledTabs n c $  _vert c ||| _horiz c ||| Full

-- _tabs = named "Tabs" Simplest

{-tiledTabs (TTC s t) l = G.group (tabBar s t Top Simplest) l-}
tiledTabs n c = G.group (_tab n c _tabs)
_tab n (TTC{tabsShrinker=s,tabsTheme=t}) =
  renamed [CutWordsLeft 1] . createTabs Always (U n) s t
-- _tab (TTC{tabsShrinker=s,tabsTheme=t}) =
--      renamed [CutWordsLeft 1] . tabBar s t Top
_tabs = named "Tabs" Simplest

-- _tab c l = renamed [CutWordsLeft 1] $ addTabs (tabsShrinker c) (tabsTheme c) l
-- _tab c l = renamed [CutWordsLeft 1] $ addTabs (tabsShrinker c) (tabsTheme c) l
_vert c = named "Vertical" $ spacing (tabSpacing c) $  Tall (vNMaster c) (vIncrement c) (vRatio c)
_horiz c = named "Horizontal" $ spacing (tabSpacing c) $ Mirror $ Tall (hNMaster c) (hIncrement c) (hRatio c)

-- | Increase the number of master groups by one
increaseNMasterGroups :: X ()
increaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN 1

-- | Decrease the number of master groups by one
decreaseNMasterGroups :: X ()
decreaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN (-1)

-- | Shrink the master area
shrinkMasterGroups :: X ()
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Expand

-- | Rotate the available outer layout algorithms
nextOuterLayout :: X ()
nextOuterLayout = sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout
