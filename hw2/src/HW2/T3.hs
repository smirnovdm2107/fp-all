module HW2.T3
  ( epart
  , mcat
  ) where

import           Data.Foldable

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

toTuple :: (Monoid a, Monoid b) => Either a b -> (a, b)
toTuple (Left a)  = (a, mempty)
toTuple (Right b) = (mempty, b)
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toTuple
