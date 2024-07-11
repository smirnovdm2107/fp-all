module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

data Option a = None | Some a
  deriving Show

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some a) = Some (f a)

data Pair a = P a a
  deriving Show

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

data Quad a = Q a a a a
  deriving Show

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e
  deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a)   = High (f a)

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> s) = f a :> mapStream f s

data List a = Nil | a :. List a
  deriving Show

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil      = Nil
mapList f (h :. t) = f h :. mapList f t

newtype Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch left val right) = Branch (mapTree f left) (f val) (mapTree f right)


instance (Eq a) => Eq (Option a) where
    None == None = True
    None == _ = False
    _ == None = False
    Some a == Some b = a == b

instance (Eq a) => Eq (Pair a) where
    P a1 a2 == P b1 b2 = a1 == b1 && a2 == b2

instance (Eq a) => Eq (Quad a) where
    Q a1 a2 a3 a4 == Q b1 b2 b3 b4 = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

instance (Eq e, Eq a) => Eq (Annotated e a) where
    (a1 :# e1) == (a2 :# e2) = e1 == e2 && a1 == a2

instance (Eq e, Eq a) => Eq (Except e a) where
    Error e1 == Error e2 = e1 == e2
    Success a1 == Success a2 = a1 == a2
    _ == _ = False

instance (Eq a) => Eq (Prioritised a) where
  Low a1 == Low a2 = a1 == a2
  Medium a1 == Medium a2 = a1 == a2
  High a1 == High a2 = a1 == a2
  _ == _ = False

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  h1 :. t1 == h2 :. t2 = h1 == h2 && t1 == t2
  _ == _ = False

instance (Eq a) => Eq (Tree a) where
  Leaf == Leaf = True
  Branch l1 el1 r1 == Branch l2 el2 r2 = l1 == l2 && el1 == el2 && r1 == r2
  _ == _ = False