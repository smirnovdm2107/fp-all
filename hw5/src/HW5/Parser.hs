{-# LANGUAGE LambdaCase #-}
module HW5.Parser
  ( parse
  ) where

import           Control.Applicative            (optional, many, (<|>))
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Control.Monad.Except           ()
import qualified Data.ByteString                as B
import           Data.Char                      (isAlphaNum, isAlpha)
import           Data.Scientific                ()
import           Data.Sequence                  ()
import qualified Data.Text                      as T (pack)
import           Data.Void                      (Void)
import           Data.List                      (intercalate)
import           Data.Word
import           HW5.Base
import qualified Text.Megaparsec                as M (Parsec, count, eof,
                                                      manyTill, notFollowedBy,
                                                      parse, satisfy, sepBy, sepBy1,
                                                      sepEndBy, try)
import qualified Text.Megaparsec.Char           as C (char, hexDigitChar,
                                                      space1)
import qualified Text.Megaparsec.Char.Lexer     as L (charLiteral, lexeme,
                                                      scientific, signed,
                                                      skipBlockComment,
                                                      skipLineComment, space,
                                                      symbol)
import           Text.Megaparsec.Error          (ParseErrorBundle)
import           Text.Read                      (readMaybe)

type Parser = M.Parsec Void String

sc :: Parser ()
sc = L.space
  C.space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

hiNum :: Parser HiValue
hiNum = HiValueNumber . toRational <$> L.signed sc (lexeme L.scientific)

string :: Parser String
string =  sc >> C.char '"' >> M.manyTill L.charLiteral (C.char '"') >>= \s -> sc >> return s

hiString :: Parser HiValue
hiString = HiValueString . T.pack <$> string


hiBool :: Parser HiValue
hiBool = boolHelper "false" False <|> boolHelper "true" True
  where
    boolHelper :: String -> Bool -> Parser HiValue
    boolHelper str b = symbol str >> (return . HiValueBool) b

hiFun :: Parser HiValue
hiFun =
  hiFunHelper "add" HiFunAdd
  <|> hiFunHelper "sub" HiFunSub
  <|> hiFunHelper "mul" HiFunMul
  <|> hiFunHelper "div" HiFunDiv
  <|> hiFunHelper "and" HiFunAnd
  <|> hiFunHelper "or" HiFunOr
  <|> hiFunHelper "not-equals" HiFunNotEquals
  <|> hiFunHelper "equals" HiFunEquals
  <|> hiFunHelper "less-than" HiFunLessThan
  <|> hiFunHelper "greater-than" HiFunGreaterThan
  <|> hiFunHelper "not-less-than" HiFunNotLessThan
  <|> hiFunHelper "not-greater-than" HiFunNotGreaterThan
  <|> hiFunHelper "if" HiFunIf
  <|> hiFunHelper "length" HiFunLength
  <|> hiFunHelper "to-upper" HiFunToUpper
  <|> hiFunHelper "to-lower" HiFunToLower
  <|> hiFunHelper "reverse" HiFunReverse
  <|> hiFunHelper "trim" HiFunTrim
  <|> hiFunHelper "list" HiFunList
  <|> hiFunHelper "range" HiFunRange
  <|> hiFunHelper "fold" HiFunFold
  <|> hiFunHelper "pack-bytes" HiFunPackBytes
  <|> hiFunHelper "unpack-bytes" HiFunUnpackBytes
  <|> hiFunHelper "zip" HiFunZip
  <|> hiFunHelper "unzip" HiFunUnzip
  <|> hiFunHelper "encode-utf8" HiFunEncodeUtf8
  <|> hiFunHelper "decode-utf8" HiFunDecodeUtf8
  <|> hiFunHelper "serialise" HiFunSerialise
  <|> hiFunHelper "deserialise" HiFunDeserialise
  <|> hiFunHelper "read" HiFunRead
  <|> hiFunHelper "write" HiFunWrite
  <|> hiFunHelper "cd" HiFunChDir
  <|> hiFunHelper "mkdir" HiFunMkDir
  <|> hiFunHelper "not" HiFunNot
  <|> hiFunHelper "parse-time" HiFunParseTime
  <|> hiFunHelper "rand" HiFunRand
  <|> hiFunHelper "echo" HiFunEcho
  <|> hiFunHelper "count" HiFunCount
  <|> hiFunHelper "keys" HiFunKeys
  <|> hiFunHelper "values" HiFunValues
  <|> hiFunHelper "invert" HiFunInvert
  where
    hiFunHelper :: String -> HiFun -> Parser HiValue
    hiFunHelper name fun = M.try (symbol name >> return (HiValueFunction fun))

byte8 :: Parser Word8
byte8 = do
  sc
  hexStr <- M.count 2 C.hexDigitChar
  case readMaybe $ "0x" ++ hexStr of
    Just word -> return word
    Nothing   -> fail "hex digit expected"

byteString :: Parser B.ByteString
byteString = do
  void (symbol "[#")
  bytes <- M.sepEndBy byte8 (C.space1 >> sc)
  void (symbol "#]")
  return (B.pack bytes)

hiByteString :: Parser HiValue
hiByteString = HiValueBytes <$> byteString

hiBracketExpr :: Parser HiExpr
hiBracketExpr = symbol "(" >> hiExpr >>= \e -> symbol ")" >> return e

hiNull :: Parser HiValue
hiNull = symbol "null" >> return HiValueNull

type Prefix = String
type Suffix = String
type Sep = String
parseContainer :: Prefix -> Suffix -> Sep ->  Parser a -> Parser [a]
parseContainer pref suff sep p = symbol pref >> p `M.sepBy` symbol sep >>= \args -> symbol suff >> return args


hiCwd :: Parser HiValue
hiCwd = symbol "cwd" >> return (HiValueAction HiActionCwd)

hiNow :: Parser HiValue
hiNow = symbol "now" >> return (HiValueAction HiActionNow)

tryRun :: HiExpr -> Parser HiExpr
tryRun a = optional (symbol "!") >>= \case
  Nothing -> return a
  Just _ -> return $ HiExprRun a

hiKey :: Parser (HiExpr, HiExpr)
hiKey = hiExpr >>= \k -> symbol ":" >> hiExpr >>= \e -> return (k, e)

hiMap :: Parser HiExpr
hiMap = HiExprDict <$> parseContainer "{" "}" "," hiKey

term :: Parser HiExpr
term =  (M.try (HiExprValue <$> hiValue) <|> M.try hiList <|> M.try hiBracketExpr <|> M.try hiMap) >>= withArgs >>= tryRun

hiValue :: Parser HiValue
hiValue =
   M.try hiFun
  <|> M.try hiNum
  <|> M.try hiBool
  <|> M.try hiString
  <|> M.try hiNull
  <|> M.try hiByteString
  <|> M.try hiCwd
  <|> M.try hiNow

hiList :: Parser HiExpr
hiList = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> parseContainer "[" "]" "," hiExpr

withArgs :: HiExpr -> Parser HiExpr
withArgs e = ((M.try brackets <|> M.try dot) >>= withArgs) <|> return e
  where
    args = parseContainer "(" ")" "," hiExpr
    brackets = HiExprApply e <$> args
    dot = HiExprApply e . (:[]) . HiExprValue . HiValueString . T.pack . intercalate "-" <$> (symbol "." >> ((:) <$> M.satisfy isAlpha <*> many (M.satisfy isAlphaNum)) `M.sepBy1` C.char '-')

applyOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
applyOp f e1 e2 = HiExprApply (HiExprValue . HiValueFunction $ f) [e1, e2]

binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary priority name f = priority (applyOp f <$ symbol name)

binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL = binary InfixL
binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR = binary InfixR
binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN = binary InfixN

table :: [[Operator Parser HiExpr]]
table = [[ InfixL (applyOp HiFunDiv <$ M.try (symbol "/" >> M.notFollowedBy (C.char '=')))
        , binaryL  "*"  HiFunMul ]
        , [ binaryL "+" HiFunAdd
        , binaryL  "-" HiFunSub]
        , [ binaryN ">=" HiFunNotLessThan
        , binaryN "<=" HiFunNotGreaterThan
        , binaryN  "<"  HiFunLessThan
        , binaryN  ">"  HiFunGreaterThan
        , binaryN "==" HiFunEquals
        , binaryN "/=" HiFunNotEquals]
        , [ binaryR "&&" HiFunAnd ]
        , [binaryR "||" HiFunOr ]]

infixExpr :: Parser HiExpr
infixExpr = makeExprParser term table

hiExpr :: Parser HiExpr
hiExpr = infixExpr

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = M.parse (hiExpr >>= \res -> sc >> M.eof >> return res)  ""
