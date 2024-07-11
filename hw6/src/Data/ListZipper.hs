{-# LANGUAGE InstanceSigs #-}

module Data.ListZipper
  ( ListZipper (..)
    , listLeft
    , listRight
    , listWrite
    , toList
    , genericMove
  ) where

import           Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ left x right) = LZ (fmap f left) (f x) (fmap f right)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight


iterateTail :: (a -> a) -> a -> [a]
iterateTail f = drop 1 . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

listLeft, listRight :: ListZipper a -> ListZipper a

listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft lz                 = lz

listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight lz                 = lz

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ left _ right) = LZ left x right

toList :: ListZipper a -> Int -> [a]
toList (LZ left x right) n = reverse (take n left) ++ [x] ++ take n right
