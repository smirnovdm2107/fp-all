module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import           Control.Monad (ap)
import           HW4.Types

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f exceptState = ES $ \s -> case runES exceptState s of
  Success (a :# s') -> Success (f a :# s')
  Error e           -> Error e

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState exceptState = ES $ \s -> case runES exceptState s of
  Success (exceptState' :# s') -> runES exceptState' s'
  Error r                      -> Error r

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) exceptState f = joinExceptState (fmap f exceptState)

data EvaluationError = DivideByZero
  deriving Show


type UnaryConstructor  = Double -> Prim Double
type BinaryConstructor = Double -> Double -> Prim Double
type UnaryOp = Double -> Double
type BinaryOp = Double -> Double -> Double

evalUnary :: Expr -> UnaryConstructor -> UnaryOp -> ExceptState EvaluationError [Prim Double] Double
evalUnary expr constructor op = do
  a <- eval expr
  modifyExceptState (\s -> constructor a : s)
  return (op a)

evalBinary :: Expr -> Expr -> BinaryConstructor -> BinaryOp -> ExceptState EvaluationError [Prim Double] Double
evalBinary expr1 expr2 constructor op = do
  a <- eval expr1
  b <- eval expr2
  modifyExceptState (\s -> constructor a b : s)
  return (op a b)


eval :: Expr -> ExceptState EvaluationError [Prim Double] Double

eval (Val v) = return v
eval (Op (Div a b)) = do
  a_res <- eval a
  b_res <- eval b
  modifyExceptState (\s -> Div a_res b_res:s)
  if b_res == 0 then throwExceptState DivideByZero else return (a_res / b_res)
eval (Op (Add a b)) = evalBinary a b Add (+)
eval (Op (Sub a b)) = evalBinary a b Sub (-)
eval (Op (Mul a b)) = evalBinary a b Mul (*)
eval (Op (Sgn a)) = evalUnary a Sgn signum
eval (Op (Abs a)) = evalUnary a Abs abs
