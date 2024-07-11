{-# LANGUAGE InstanceSigs #-}
module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import           HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S (mapAnnotated f . runS state)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState state = S $ \s -> case runS state s of
  (a :# s') -> runS a s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) k a = k >>= \s1 -> a >>= \s2 -> return (s1 s2)
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) state k = joinState (mapState k state)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show, Eq)

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs = Op . Abs 
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational = Val . fromRational

type UnaryCalc = Double -> Double
type BinaryCalc = Double -> Double -> Double
type BinaryOp = Double -> Double -> Prim Double
type UnaryOp = Double -> Prim Double 

eval :: Expr -> State [Prim Double] Double
eval (Val val) = pure val
eval (Op (Add a b)) = binary Add (+) a b
eval (Op (Sub a b)) = binary Sub (-) a b
eval (Op (Mul a b)) = binary Mul (*) a b
eval (Op (Div a b)) = binary Div (/) a b
eval (Op (Abs a)) = unary Abs abs a
eval (Op (Sgn a)) = unary Sgn signum a

unary :: UnaryOp -> UnaryCalc -> Expr -> State [Prim Double] Double
unary op f expr = do
  result <- eval expr
  modifyState (\s -> op result : s)
  return (f result)

binary :: BinaryOp -> BinaryCalc -> Expr -> Expr -> State [Prim Double] Double
binary op f expr1 expr2 = do
  left <- eval expr1
  right <- eval expr2
  modifyState (\s -> op left right : s)
  return (f left right)