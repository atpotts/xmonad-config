{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module GroupMotions where
  
import XMonad
import XMonad.Util.Stack
import XMonad.StackSet (Stack(..))
import XMonad.Layout.Groups
import Control.Monad (replicateM_)

import qualified Data.Set as S
import Data.Function ((&))
import Control.Arrow (first,(>>>))

import Data.List (splitAt)

-- General Infrastructure--
----------------------------

popz :: Zipper a                -> (Maybe a,Zipper a)
popz Nothing                     = (Nothing,Nothing)
popz (Just (Stack f [] [] ))     = (Just f,Nothing)
popz (Just (Stack f (x:xs) [] )) = (Just f,Just $ Stack x xs [])
popz (Just (Stack f ups (x:xs))) = (Just f, Just $ Stack x ups xs)

putz :: Maybe a -> Zipper a             -> Zipper a
putz Nothing x                           = x
putz (Just a) Nothing                    = Just $ Stack a [] []
putz (Just a) (Just (Stack f ups downs)) = Just (Stack a ups (f:downs))

withGroup :: Functor f => (Zipper a -> f (Zipper a)) -> Group l a -> f (Group l a)
withGroup f (G l x) = G l <$> f x

rotateTo :: Int -> Zipper n -> Zipper n
rotateTo _ Nothing = Nothing
rotateTo n (Just (Stack f up down)) =
  let (up',down') = splitAt (n-1) $ reverse up ++ down
      in Just $ Stack f (reverse up') down'

-- monad instance of tuple isn't quite good enough. Easiest to do this
-- as otherwise we end up with a pointless monoid
onFocusedZX :: (x -> (Maybe a,x)) -> Zipper x -> (Maybe a,Zipper x)
onFocusedZX f Nothing = (Nothing, Nothing)
onFocusedZX f (Just (Stack x up dn)) =
  fmap (\x' -> Just (Stack x' up dn)) $ f x

-- Infrastructure for Absolute Motions
---------------------------------------
--
moveToGroup :: Int -> Zipper (Group l a) -> Zipper (Group l a)
moveToGroup _ Nothing = Nothing
moveToGroup n stack =
  let (f,windows) = onFocusedZX (withGroup popz) stack
  in fromIndex (fst $ toIndex windows) (n-1)
   & onFocusedZ (onZipper  $ putz f)

rotateInGroup :: Int -> Zipper (Group l a) -> Zipper (Group l a)
rotateInGroup n = onFocusedZ (onZipper $ rotateTo n)

focusN :: Int -> Zipper n -> Zipper n
focusN _ Nothing = Nothing
focusN n z       = fromIndex (fst $ toIndex z) (n -1)

focusInGroup:: Int -> Zipper (Group l a) -> Zipper (Group l a)
focusInGroup n = onFocusedZ (onZipper $ focusN n)

modgroups :: (forall l a . Zipper (Group l a) -> Zipper (Group l a)) -> X()
modgroups f = sendMessage (Modify (\_ a -> f a)) >> sendMessage Refocus

modgroupsn :: (forall l a . Zipper (Group l a) -> Zipper (Group l a)) -> Int -> X()
modgroupsn f n = sendMessage (Modify (\_ a -> iterate f a !! n))
              >> sendMessage Refocus

modgroupsXn :: X () -> Int -> X ()
modgroupsXn x n = do
  replicateM_ n x >> sendMessage Refocus

modgroupsXn1 :: X () -> X () -> Int -> X ()
modgroupsXn1 x1 x2 n = do
  replicateM_ (n-1) x1 >> x2 >> sendMessage Refocus

-- Infrastructure for relative motions
---------------------------------------

isLast :: Zipper a -> Bool
isLast (Just (Stack f _ [])) = True
isLast _ = False

isFirst :: Zipper a -> Bool
isFirst (Just (Stack f [] _)) = True
isFirst _ = False

focusFirst :: Zipper a -> Zipper a
focusFirst z = fromIndex (fst$toIndex z) 0

focusLast z = fromTags $ f (fst $ toIndex z)
 where f [] = []
       f (x:[]) = [Right x]
       f (x:xs) = Left x : f xs

moveZ :: (Zipper a -> Bool) -- first, work out whether to switch bounadris
      -> (Zipper a -> Zipper a) -- work to do within group
      -> (Zipper (Group l a) -> Zipper (Group l a)) --between groups
      -> Zipper (Group l a) -> Zipper (Group l a) --result

moveZ p inside outside zipper =
    let runpred z = case p z of
          True -> (Just (),z)
          False -> (Nothing, inside z)
        (x ,rest) = onFocusedZX (withGroup runpred) zipper
    in case x of
     (Just ()) -> outside rest
     Nothing -> rest

oz f = onFocusedZ (onZipper f)

fNext = oz focusDownZ -- moveZ isLast focusDownZ (focusDownZ >>> oz focusFirst)
fPrev = oz focusUpZ
--moveZ isFirst focusUpZ (focusUpZ >>> oz focusLast)
sNext = moveZ isLast swapDownZ (\z -> case onFocusedZX (withGroup popz) z of
  (Nothing,rest) -> rest
  (Just w,rest) -> focusDownZ rest & oz (focusFirst >>> insertUpZ w))
sPrev = moveZ isFirst swapUpZ (\z -> case onFocusedZX (withGroup popz) z of
  (Nothing,rest) -> rest
  (Just w,rest) -> focusUpZ rest & oz (focusLast >>> insertDownZ w))


pullout :: [ Window ] -> ModifySpec
pullout xs l ss = flip fromIndex 1
  $ G l (fromIndex xs 1)
  : flip map (fst $ toIndex ss) (
      onZipper (filterZ (const $ not.(`elem` (S.fromList xs)))))
