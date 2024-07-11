module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ e Leaf               = e
tfoldr f e (Branch _ l cur r) = tfoldr f (f cur (tfoldr f e r)) l
