{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import           Control.Applicative
import           Control.Monad
import           Numeric.Natural     (Natural)

import           HW4.T1              (ExceptState (..))
import           HW4.Types

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P exceptState) str = case runES exceptState (0, str) of
  Success (a :# _) -> Success a
  Error e          -> Error e

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (<|>) (P exceptState1) (P exceptState2) = P $ ES $ \s -> case runES exceptState1 s of
    Success res -> Success res
    Error _     -> runES exceptState2 s

-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \state@(pos, s) -> case s of
  [] -> Success (() :# state)
  _  -> Error $ ErrorAtPos pos

predicateChar :: (Char -> Bool) -> Parser Char
predicateChar predicate = do
  ch <- pChar
  if predicate ch then return ch else empty

oneOfChars :: [Char] -> Parser Char
oneOfChars l = predicateChar (`elem` l)

parseChar :: Char -> Parser Char
parseChar ch = predicateChar (== ch)

skip :: [Char] -> Parser ()
skip chs = do
  _ <- parseChars chs
  return ()

parseChars :: [Char] -> Parser [Char]
parseChars = mapM parseChar

digit :: Parser Int
digit = do
  d <- oneOfChars ['0'..'9']
  return (fromEnum d - fromEnum '0')

digits :: Parser [Int]
digits = some digit

trySkip :: [Char] -> Parser ()
trySkip chs = do
  _ <- optional (skip chs)
  return ()

pow :: (Num a, Num b, Eq b) => b -> a -> a
pow 0 _ = 1
pow c n = n * pow (c - 1) n


number :: Parser Expr
number = token $ do
  n1 <- digits
  n2 <- optional (trySkip "." >> digits)
  -- getting rational from big integer to save precision
  return $ Val $ fromRational (toRational (foldInteger (n1 ++ fraction n2)) / pow (length (fraction n2)) 10)
    where
      foldInteger :: [Int] -> Integer
      foldInteger = foldl (\e d -> e * 10 + toInteger d) (0 :: Integer)

      fraction :: Maybe [Int] -> [Int]
      fraction Nothing  = []
      fraction (Just l) = l
-- anything surrounded with
token :: Parser a -> Parser a
token parser = do
  ws
  r <- parser
  ws
  return r

-- (E)
brackets :: Parser Expr
brackets = do
  _ <- token (parseChar '(')
  e <- expr
  _ <- token (parseChar ')')
  return e

-- T -> (E) | n
prim :: Parser Expr
prim = number <|> brackets

-- skip whitespaces
ws :: Parser ()
ws = do
  _ <- many (parseChar ' ')
  return ()

-- *, - , + , *
type Sign = [Char]
-- Add Sub Mul Div
type BinaryOp = Expr -> Expr -> Prim Expr

parseSign :: [Char] -> Parser ()
parseSign chs = do
  _ <- token (parseChars chs)
  return ()

-- T -> P Rt
term :: Parser Expr
term = onePriorityOp prim Term

-- Rt -> sign T Rt
termOpRest :: Sign -> BinaryOp -> Expr -> Parser Expr
termOpRest = onePriorityOpRest prim Term


multiplyRest :: Expr -> Parser Expr
multiplyRest = termOpRest "*" Mul

divideRest :: Expr -> Parser Expr
divideRest = termOpRest "/" Div

-- Rt -> * T Rt | / T Rt | <empty>
termRest :: Expr -> Parser Expr
termRest t = multiplyRest t <|> divideRest t <|> return t

-- E -> T R
expr :: Parser Expr
expr = onePriorityOp term Expr

subtrRest :: Expr -> Parser Expr
subtrRest = exprOpRest "-" Sub

additionRest :: Expr -> Parser Expr
additionRest = exprOpRest "+" Add

-- Re -> - T Re | + T R e | <empty>
exprRest :: Expr -> Parser Expr
exprRest e = subtrRest e <|> additionRest e <|> return e

-- Re -> sign T Re
exprOpRest :: Sign -> BinaryOp -> Expr -> Parser Expr
exprOpRest = onePriorityOpRest term Expr


data OpPriority = Expr | Term

onePriorityOp :: Parser Expr -> OpPriority -> Parser Expr
onePriorityOp next priority = do
  arg1 <- next
  onePriorityRest priority arg1

onePriorityRest :: OpPriority -> Expr -> Parser Expr
onePriorityRest Expr = exprRest
onePriorityRest Term = termRest

onePriorityOpRest :: Parser Expr -> OpPriority -> Sign -> BinaryOp -> Expr -> Parser Expr
onePriorityOpRest next priority sign op arg1 = do
  parseSign sign
  arg2 <- next
  onePriorityRest priority (Op (op arg1 arg2))

-- E <eof>
mathExpr :: Parser Expr
mathExpr = do
  e <- expr
  pEof
  return e

parseExpr :: String -> Except ParseError Expr
parseExpr = runP mathExpr
