{-# LANGUAGE InstanceSigs #-}

module Data.Grid
  ( Grid (..)
    , up
    , down
    , left
    , right
    , gridRead
    , gridWrite
    , horizontal
    , vertical
    , toList
  ) where

import           Control.Comonad (Comonad (..))

import           Data.ListZipper (ListZipper (..), genericMove, listLeft,
                                  listRight, listWrite)
import qualified Data.ListZipper as DL (toList)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up, down :: Grid a -> Grid a
up (Grid lz) = Grid (listLeft lz)
down (Grid lz) = Grid (listRight lz)

left, right :: Grid a -> Grid a
left (Grid lz) = Grid (fmap listLeft lz)
right (Grid lz) = Grid (fmap listRight lz)

gridRead :: Grid a -> a
gridRead (Grid (LZ _ (LZ _ x _) _)) = x

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical = genericMove up down

toList :: Grid a -> Int -> [[a]]
toList (Grid lz) n = toList' $ fmap toList' lz
  where
    toList' = flip DL.toList n


instance Functor Grid where
  fmap f (Grid lz) = Grid $ fmap (fmap f) lz

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical
