{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

----------------------------------------------------------------------
-- |
-- Module      : XMonad.Actions.GroupNavigation
-- Copyright   : (c) nzeh@cs.dal.ca
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : nzeh@cs.dal.ca
-- Stability   : unstable
-- Portability : unportable
--
-- Provides methods for cycling through groups of windows across
-- workspaces, ignoring windows that do not belong to this group.  A
-- group consists of all windows matching a user-provided boolean
-- query.
--
-- Also provides a method for jumping back to the most recently used
-- window in any given group.
--
-- Modified: Alistair Potts 22/7/2018
----------------------------------------------------------------------

module ContribMod.GroupNavigation ( -- * Usage
                                        -- $usage
                                        Direction
                                      , Onward (..)
                                      , History (..)
                                      , nextMatch
                                      , nextMatchOrDo
                                      , nextMatchWithThis
                                      , historyHook
                                      ) where

import Control.Monad.Reader
import Data.Foldable as Fold
import Data.Map as Map
import Data.Either
import Data.Sequence as Seq
import Data.Set as Set
import Control.Arrow (first, second)
import Graphics.X11.Types
import Prelude hiding (concatMap, drop, elem, filter, null, reverse)
import XMonad.Core
import XMonad.ManageHook
import XMonad.Operations (windows, withFocused)
import qualified XMonad.StackSet as SS
import qualified XMonad.Util.ExtensibleState as XS

{- $usage

Import the module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.GroupNavigation

To support cycling forward and backward through all xterm windows, add
something like this to your keybindings:

> , ((modm              , xK_t), nextMatch Forward  (className =? "XTerm"))
> , ((modm .|. shiftMask, xK_t), nextMatch Backward (className =? "XTerm"))

These key combinations do nothing if there is no xterm window open.
If you rather want to open a new xterm window if there is no open
xterm window, use 'nextMatchOrDo' instead:

> , ((modm              , xK_t), nextMatchOrDo Forward  (className =? "XTerm") (spawn "xterm"))
> , ((modm .|. shiftMask, xK_t), nextMatchOrDo Backward (className =? "XTerm") (spawn "xterm"))

You can use 'nextMatchWithThis' with an arbitrary query to cycle
through all windows for which this query returns the same value as the
current window.  For example, to cycle through all windows in the same
window class as the current window use:

> , ((modm , xK_f), nextMatchWithThis Forward  className)
> , ((modm , xK_b), nextMatchWithThis Backward className)

Finally, you can define keybindings to jump to the most recent window
matching a certain Boolean query.  To do this, you need to add
'historyHook' to your logHook:

> main = xmonad $ def { logHook = historyHook }

Then the following keybindings, for example, allow you to return to
the most recent xterm or emacs window or to simply to the most recent
window:

> , ((modm .|. controlMask, xK_e),         nextMatch History (className =? "Emacs"))
> , ((modm .|. controlMask, xK_t),         nextMatch History (className =? "XTerm"))
> , ((modm                , xK_BackSpace), nextMatch History (return True))

Again, you can use 'nextMatchOrDo' instead of 'nextMatch' if you want
to execute an action if no window matching the query exists. -}

--- Basic cyclic navigation based on queries -------------------------


seqPartitionM :: Monad m => (a -> m Bool) -> Seq a -> m (Seq a, Seq a)
seqPartitionM p s = do
  xs' <- forM s $ \x -> do
       x' <- p x
       return $ if x' then Right x else Left x
  let (a,b) = Seq.partition isRight xs'
  return (fmap unmark a, fmap unmark b)


class Direction x where
  orderedWindowList :: x -> X (Seq Window)

data History = BackwardsHistory | AllHistory | ForwardsHistory
  | BackwardsWhen (Query Bool)
instance Direction History where
  orderedWindowList = \case
      BackwardsHistory -> markHistory Left >> go'
        ( uncurry (><)
        . second (Seq.reverse)
        . Seq.partition isRight
        )
      ForwardsHistory -> markHistory Right >> go'
        ( uncurry (><)
        . second (Seq.reverse)
        . Seq.partition isLeft
        )
      AllHistory -> markHistory Left >> go' id
      BackwardsWhen q -> do
        markHistory Left
        go $ \x -> do
          (lft,rgt) <- flip seqPartitionM x $ \w -> do
                  b <- runQuery q $ unmark w
                  return (isRight w && b)
          return $ lft >< Seq.reverse rgt
    where go' f = go (return.f)
          go f  = do
            (HistoryDB w ws)  <- XS.get
            ws' <- f ws
            return $ fmap unmark ws'


reLeft :: X()
reLeft = do
  (HistoryDB w ws) <- XS.get
  XS.put $ HistoryDB (fmap (Left . unmark) w) (fmap (Left . unmark) ws)


data Onward = Forward | Backward
instance Direction Onward where
  orderedWindowList dir     = withWindowSet $ \ss -> do
    wsids <- asks (Seq.fromList . workspaces . config)
    let wspcs = orderedWorkspaceList ss wsids
        wins  = dirfun dir
                $ Fold.foldl' (><) Seq.empty
                $ fmap (Seq.fromList . SS.integrate' . SS.stack) wspcs
        cur   = SS.peek ss
    return $ maybe wins (rotfun wins) cur
    where
      dirfun Backward = Seq.reverse
      dirfun _        = id
      rotfun wins x   = rotate $ rotateTo (== x) wins

-- | Focuses the next window for which the given query produces the
-- same result as the currently focused window.  Does nothing if there
-- is no focused window (i.e., the current workspace is empty).
nextMatchWithThis :: (Eq a, Direction d) => d -> Query a -> X ()
nextMatchWithThis dir qry = withFocused $ \win -> do
  prop <- runQuery qry win
  nextMatch dir (qry =? prop)

-- | Focuses the next window that matches the given boolean query.
-- Does nothing if there is no such window.  This is the same as
-- 'nextMatchOrDo' with alternate action @return ()@.
nextMatch :: (Direction d) => d -> Query Bool -> X ()
nextMatch dir qry = nextMatchOrDo dir qry (return ())

-- | Focuses the next window that matches the given boolean query.  If
-- there is no such window, perform the given action instead.
nextMatchOrDo :: (Direction d) => d -> Query Bool -> X () -> X ()
nextMatchOrDo dir qry act = orderedWindowList dir
                            >>= focusNextMatchOrDo qry act

-- Produces the action to perform depending on whether there's a
-- matching window
focusNextMatchOrDo :: Query Bool -> X () -> Seq Window -> X ()
focusNextMatchOrDo qry act = findM (runQuery qry)
                             >=> maybe act (windows . SS.focusWindow)

-- Returns the list of windows ordered by workspace as specified in
-- ~/.xmonad/xmonad.hs


-- Returns the ordered workspace list as specified in ~/.xmonad/xmonad.hs
orderedWorkspaceList :: WindowSet -> Seq String -> Seq WindowSpace
orderedWorkspaceList ss wsids = rotateTo isCurWS wspcs'
    where
      wspcs      = SS.workspaces ss
      wspcsMap   = Fold.foldl' (\m ws -> Map.insert (SS.tag ws) ws m) Map.empty wspcs
      wspcs'     = fmap (\wsid -> wspcsMap ! wsid) wsids
      isCurWS ws = SS.tag ws == SS.tag (SS.workspace $ SS.current ss)

--- History navigation, requires a layout modifier -------------------

-- The state extension that holds the history information
type HistoryWindow = Either Window Window
data HistoryDB = HistoryDB (Maybe HistoryWindow) -- currently focused window
                           (Seq HistoryWindow)   -- previously focused windows
               deriving (Read, Show, Typeable)

instance ExtensionClass HistoryDB where
    initialValue  = HistoryDB Nothing Seq.empty
    extensionType = PersistentExtension

-- | Action that needs to be executed as a logHook to maintain the
-- focus history of all windows as the WindowSet changes.
historyHook :: X ()
historyHook = XS.get >>= updateHistory >>= XS.put

unmark :: Either a a -> a
unmark = either id id

markHistory :: (Window -> HistoryWindow) -> X()
markHistory mark = do
  HistoryDB x xs <- XS.get
  XS.put $ HistoryDB (mark .  unmark <$> x) xs

-- Updates the history in response to a WindowSet change
updateHistory :: HistoryDB -> X HistoryDB
updateHistory (HistoryDB oldcur oldhist) = withWindowSet $ \ss -> do
  let newcur   = SS.peek ss
      wins     = Set.fromList $ SS.allWindows ss
      newhist  = Seq.filter (flip Set.member wins . either id id) (ins oldcur oldhist)
  return $ HistoryDB (Right <$> newcur) (del newcur newhist)
  where
    ins x xs = maybe xs (<| xs) x
    del x xs = maybe xs (\x' -> Seq.filter ((/= x').unmark) xs) x

--- Some sequence helpers --------------------------------------------

-- Rotates the sequence by one position
rotate :: Seq a -> Seq a
rotate xs = rotate' (viewl xs)
  where
    rotate' EmptyL      = Seq.empty
    rotate' (x' :< xs') = xs' |> x'

-- Rotates the sequence until an element matching the given condition
-- is at the beginning of the sequence.
rotateTo :: (a -> Bool) -> Seq a -> Seq a
rotateTo cond xs = let (lxs, rxs) = Seq.breakl cond xs in rxs >< lxs

--- A monadic find ---------------------------------------------------

-- Applies the given action to every sequence element in turn until
-- the first element is found for which the action returns true.  The
-- remaining elements in the sequence are ignored.
findM :: Monad m => (a -> m Bool) -> Seq a -> m (Maybe a)
findM cond xs = findM' cond (viewl xs)
  where
    findM' _   EmptyL      = return Nothing
    findM' qry (x' :< xs') = do
      isMatch <- qry x'
      if isMatch
        then return (Just x')
        else findM qry xs'
