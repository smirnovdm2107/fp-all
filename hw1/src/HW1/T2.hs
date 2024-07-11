module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import           Numeric.Natural

data N = Z | S N deriving (Show, Eq)

nplus :: N -> N -> N
nplus Z n       = n
nplus (S n1) n2 = nplus n1 (S n2)

nmult :: N -> N -> N
nmult Z _       = Z
nmult _ Z       = Z
nmult (S n1) n2 = nmult n1 n2 `nplus` n2

nsub :: N -> N -> Maybe N
nsub n Z           = Just n
nsub Z _           = Nothing
nsub (S n1) (S n2) = nsub n1 n2

ncmp :: N -> N -> Ordering
ncmp n1 n2 = case nsub n1 n2 of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S . nFromNatural . pred $ n

nToNatural :: N -> Natural
nToNatural Z     = 0
nToNatural (S n) = succ . nToNatural $ n

nToNum :: Num a => N -> a
nToNum = fromInteger . toInteger . nToNatural

nEven :: N -> Bool
nEven Z     = True
nEven (S n) = not . nEven $ n

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv Z _ = Z
ndiv n1 n2 = case nsub n1 n2 of
  Nothing -> Z
  Just n  -> S (ndiv n n2)

nmod :: N -> N -> N
nmod n1 n2 = case nsub n1 n2 of
  Nothing -> n1
  Just n  -> nmod n n2
