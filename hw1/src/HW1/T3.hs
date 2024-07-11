module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Height = Int
type Size = Int
type Meta = (Size, Height)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (s, _) _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf                  = 0
tdepth (Branch (_, h) _ _ _) = h

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember el (Branch _ l cur r)
  | el < cur = tmember el l
  | el > cur = tmember el r
  | otherwise = el == cur

tdiff :: Tree a -> Int
tdiff Leaf             = 0
tdiff (Branch _ l _ r) = tdepth l - tdepth r

updateMeta :: Tree a -> Tree a
updateMeta Leaf = Leaf
updateMeta (Branch _ l cur r) = Branch (tsize l + tsize r + 1, max (tdepth l) (tdepth r) + 1) l cur r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert el Leaf = Branch (1, 1) Leaf el Leaf
tinsert el t@(Branch m l cur r)
  | el < cur = balance . updateMeta $ Branch m (tinsert el l) cur r
  | el > cur = balance . updateMeta $ Branch m l cur (tinsert el r)
  | otherwise = t

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance t@(Branch _ l _ r)
  | -2 < diff && diff < 2 = t
  | diff == -2 && (rdiff == -1 || rdiff == 0) = smallLeft t
  | diff == -2 = bigLeft t
  | diff == 2 && (ldiff == 1 || ldiff == 0) = smallRight t
  | otherwise = bigRight t
  where
    diff = tdiff t
    ldiff = tdiff l
    rdiff = tdiff r

smallLeft :: Tree a -> Tree a
smallLeft (Branch (s, h) l cur (Branch _ l' cur' r')) = updateMeta $
  Branch (s, h) (Branch (tsize l + tsize l' + 1, max (tdepth l) (tdepth l') + 1) l cur l') cur' r'
smallLeft t = t

smallRight :: Tree a -> Tree a
smallRight (Branch (s, h) (Branch _ l' cur' r') cur r) = updateMeta $
  Branch (s, h) l' cur' (Branch (tsize r' + tsize r + 1, max (tdepth r) (tdepth r') + 1) r' cur r)
smallRight t = t

bigLeft :: Tree a -> Tree a
bigLeft (Branch (s, h) l cur (Branch _ (Branch _ l'' cur'' r'') cur' r')) = updateMeta $
  Branch (s, h - 1)
   (Branch (tsize l + tsize l'' + 1, h - 2) l cur l'')
    cur''
    (Branch (tsize r' + tsize r'' + 1, h - 2) r'' cur' r')
bigLeft t = t

bigRight :: Tree a -> Tree a
bigRight (Branch (s, h) (Branch _ l' cur' (Branch _ l'' cur'' r'')) cur r) = updateMeta $
  Branch (s, h - 1)
  (Branch (tsize l' + tsize l'' + 1, h - 2) l' cur' l'')
   cur''
    (Branch (tsize r + tsize r'' + 1, h - 2) r'' cur r)
bigRight t = t

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
