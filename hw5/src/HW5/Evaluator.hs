{-# LANGUAGE LambdaCase #-}

module HW5.Evaluator
  ( eval
  , decode
  ) where

import           Control.Applicative
import Control.Monad.Except ()
import           Control.Monad
import           Data.Foldable       (toList)
import qualified Data.Functor        as F
import           Data.Ratio ()
import Data.Tuple ()
import Data.Maybe (fromMaybe)
import           Data.Semigroup      (stimes)
import           Data.Sequence  as S     (Seq (Empty, (:<|)),(><), singleton, fromList, take, drop, length, reverse, index)
import qualified Data.Text           as T (Text, append, drop, index, length, pack,
                                           reverse, singleton,
                                           strip, take, toLower, toUpper,
                                           unpack, index, singleton)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8')
import qualified Codec.Serialise as Seria (serialise, deserialiseOrFail)
import qualified Codec.Compression.Zlib as Z
import           Data.Traversable ()
import           HW5.Base            (HiError (..), HiExpr (..), HiAction(..),
                                      HiFun (..), HiValue (..), Valuable, HiMonad(..),
                                      fromHiValue, toHiValue)
import qualified Data.ByteString as B
import qualified Data.Map as M (empty, lookup, fromListWith, foldlWithKey, insertWith, Map, keys, elems, fromList)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Control.Monad.Trans.Except
import Text.Read (readMaybe)
import Data.Time
data Arity = Unary | Binary | Ternary | List

getArity :: (HiMonad m) => HiValue -> ExceptT HiError m Arity
getArity f = case f of
  HiValueFunction HiFunAdd -> return Binary
  HiValueFunction HiFunSub -> return Binary
  HiValueFunction HiFunMul -> return Binary
  HiValueFunction HiFunDiv -> return Binary

  HiValueFunction HiFunNot -> return Unary
  HiValueFunction HiFunAnd -> return Binary
  HiValueFunction HiFunOr -> return Binary

  HiValueFunction HiFunLessThan -> return Binary
  HiValueFunction HiFunGreaterThan -> return Binary
  HiValueFunction HiFunNotLessThan -> return Binary
  HiValueFunction HiFunNotGreaterThan -> return Binary
  HiValueFunction HiFunEquals -> return Binary
  HiValueFunction HiFunNotEquals -> return Binary

  HiValueFunction HiFunIf -> return Ternary

  HiValueFunction HiFunLength -> return Unary
  HiValueFunction HiFunToUpper -> return Unary
  HiValueFunction HiFunToLower -> return Unary
  HiValueFunction HiFunReverse -> return Unary
  HiValueFunction HiFunTrim -> return Unary

  HiValueFunction HiFunList -> return List
  HiValueFunction HiFunRange -> return Binary
  HiValueFunction HiFunFold -> return Binary

  HiValueFunction HiFunPackBytes -> return Unary
  HiValueFunction HiFunUnpackBytes -> return Unary

  HiValueFunction HiFunEncodeUtf8 -> return Unary
  HiValueFunction HiFunDecodeUtf8 -> return Unary

  HiValueFunction HiFunZip -> return Unary
  HiValueFunction HiFunUnzip -> return Unary

  HiValueFunction HiFunSerialise -> return Unary
  HiValueFunction HiFunDeserialise -> return Unary

  HiValueFunction HiFunRead -> return Unary
  HiValueFunction HiFunWrite -> return Binary
  HiValueFunction HiFunMkDir -> return Unary
  HiValueFunction HiFunChDir -> return Unary

  HiValueFunction HiFunRand -> return Binary

  HiValueFunction HiFunEcho -> return Unary
  HiValueFunction HiFunCount -> return Unary
  HiValueFunction HiFunKeys -> return Unary
  HiValueFunction HiFunValues -> return Unary
  HiValueFunction HiFunInvert -> return Unary

  HiValueDict _ -> return Unary

  HiValueFunction HiFunParseTime -> return Unary

  HiValueList _ -> return List
  HiValueString _-> return List
  HiValueBytes _ -> return List
  _ -> throwE HiErrorInvalidFunction

comporess :: B.ByteString -> B.ByteString
comporess = BL.toStrict . Z.compressWith  params . BL.fromStrict
  where
    params = Z.defaultCompressParams {Z.compressLevel = Z.bestCompression}

decompress :: B.ByteString -> B.ByteString
decompress = BL.toStrict . Z.decompressWith params . BL.fromStrict
  where
    params = Z.defaultDecompressParams

binary :: (HiMonad m) => HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
binary (HiValueFunction HiFunAnd) x1 x2 = evalExpr x1 >>= \case
  HiValueBool x1' -> if not x1' then return $ toHiValue x1' else evalExpr x2
  HiValueNull -> return $ HiValueBool False
  _ -> evalExpr x2

binary (HiValueFunction HiFunOr) x1 x2 = evalExpr x1 >>= \case
  HiValueBool x1' -> if x1' then return $ toHiValue x1' else evalExpr x2
  HiValueNull -> evalExpr x2
  v -> return v
binary f x1 x2 = evalExpr x1 >>= \x1' -> evalExpr x2 >>= \x2' -> binaryEvaled f x1' x2'

unary :: (HiMonad m) => HiValue -> HiExpr -> ExceptT HiError m HiValue
unary f x = evalExpr x >>= \x' -> unaryEvaled f x'

binaryEvaled :: (HiMonad m) => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
binaryEvaled (HiValueFunction HiFunAdd) x1 x2 = applySafeBinary ((+) :: Rational -> Rational -> Rational) x1 x2
  <|> applySafeBinary ((<>) :: Seq HiValue -> Seq HiValue -> Seq HiValue) x1 x2
  <|> applySafeBinary T.append x1 x2
  <|> applySafeBinary (addUTCTime . fromRational :: Rational -> UTCTime -> UTCTime) x1 x2
  <|> applySafeBinary (flip (addUTCTime . fromRational) :: UTCTime -> Rational -> UTCTime) x1 x2
  <|> applySafeBinary B.append x1 x2
binaryEvaled (HiValueFunction HiFunSub) x1 x2 = applySafeBinary ((-) :: Rational -> Rational -> Rational) x1 x2
  <|> applySafeBinary (\a b -> toRational (diffUTCTime a b)) x1 x2
binaryEvaled (HiValueFunction HiFunMul) x1 x2 = applySafeBinary ((*) :: Rational -> Rational -> Rational ) x1 x2
  <|> applySafeBinary stimesText x1 x2 <|> applySafeBinary stimesText x2 x1
  <|> applySafeBinary stimesSeq x1 x2 <|> applySafeBinary stimesSeq x2 x1
  <|> applySafeBinary stimesBS x1 x2  <|> applySafeBinary stimesBS x2 x1
  where
    stimesText :: Int -> T.Text -> T.Text
    stimesText = stimes

    stimesSeq :: Int -> Seq HiValue -> Seq HiValue
    stimesSeq = stimes

    stimesBS :: Int -> B.ByteString -> B.ByteString
    stimesBS = stimes

binaryEvaled (HiValueFunction HiFunDiv) x1 x2 =
  let
    div' :: (HiMonad m) => Rational -> Rational -> ExceptT HiError m HiValue
    div' x y = if y == 0 then throwE HiErrorDivideByZero else return $ toHiValue (x / y)
  in
    applyBinary div' x1 x2
    <|> applySafeBinary (\x y -> x `T.append` T.pack "/" `T.append` y) x1 x2
binaryEvaled (HiValueFunction HiFunEquals) x1 x2 = applySafeBinary ((==) :: HiValue -> HiValue -> Bool ) x1 x2
binaryEvaled (HiValueFunction HiFunNotEquals) x1 x2 = applySafeBinary ((/=) :: HiValue -> HiValue -> Bool) x1 x2
binaryEvaled (HiValueFunction HiFunLessThan) x1 x2 = applySafeBinary ((<) :: HiValue -> HiValue -> Bool) x1 x2
binaryEvaled (HiValueFunction HiFunNotLessThan) x1 x2 = applySafeBinary ((>=) :: HiValue -> HiValue -> Bool) x1 x2
binaryEvaled (HiValueFunction HiFunGreaterThan) x1 x2 = applySafeBinary ((>) :: HiValue -> HiValue -> Bool) x1 x2
binaryEvaled (HiValueFunction HiFunNotGreaterThan) x1 x2 = applySafeBinary ((<=) :: HiValue -> HiValue -> Bool) x1 x2

binaryEvaled (HiValueFunction HiFunRange) x1 x2 = applySafeBinary go x1 x2
  where
    go :: Rational -> Rational -> Seq HiValue
    go from to = if from <= to then HiValueNumber from :<| go (from + 1) to else Empty

binaryEvaled (HiValueFunction HiFunFold) f args = applyBinary hiFold f args
  where
    hiFold :: (HiMonad m) => HiValue -> Seq HiValue -> ExceptT HiError m HiValue
    hiFold f' (e1 :<| e2 :<| t) = hiFold' f' (binary f' (HiExprValue e1) (HiExprValue e2)) t
    hiFold _ (e :<| Empty) = return e
    hiFold _ Empty = throwE HiErrorInvalidArgument
    hiFold' :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue -> Seq HiValue -> ExceptT HiError m HiValue
    hiFold' f' = foldl (\e a -> e >>= \e' -> binary f' (HiExprValue e') (HiExprValue a))

binaryEvaled (HiValueFunction HiFunWrite) x1 x2 = applySafeBinary actionWrite' x1 x2
  where
    actionWrite' :: T.Text -> T.Text -> HiAction
    actionWrite' filePath d = HiActionWrite (T.unpack filePath) (TE.encodeUtf8 d)

binaryEvaled (HiValueFunction HiFunRand) x1 x2 = applySafeBinary HiActionRand x1 x2

binaryEvaled _ _ _ = throwE HiErrorInvalidArgument

unaryEvaled :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
unaryEvaled (HiValueFunction HiFunNot) x  = applySafeUnary not x
unaryEvaled (HiValueFunction HiFunLength) x = applySafeUnary T.length x
  <|> applySafeUnary (S.length :: Seq HiValue -> Int) x
  <|> applySafeUnary (B.length :: B.ByteString -> Int) x
unaryEvaled (HiValueFunction HiFunToUpper) x = applySafeUnary  T.toUpper x
unaryEvaled (HiValueFunction HiFunToLower) x = applySafeUnary T.toLower x
unaryEvaled (HiValueFunction HiFunReverse) x = applySafeUnary T.reverse x
  <|> applySafeUnary (S.reverse :: Seq HiValue -> Seq HiValue) x
  <|> applySafeUnary (B.reverse :: B.ByteString -> B.ByteString) x
unaryEvaled (HiValueFunction HiFunTrim) x = applySafeUnary T.strip x
unaryEvaled (HiValueFunction HiFunPackBytes) x = applyUnary packBytes x
  where
    packBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
    packBytes s =
      (fromHiValue s :: (HiMonad m) => ExceptT HiError m (Seq HiValue)) >>= (foldr folder (return B.empty) >=> return . toHiValue)
    bytes8 :: (HiMonad m) => HiValue -> ExceptT HiError m Word8
    bytes8 b = (fromHiValue b :: (HiMonad m) => ExceptT HiError m Int) >>= \b' -> if 0 <= b' && b' < 256
       then return (fromIntegral b')
        else throwE HiErrorInvalidArgument
    folder :: (HiMonad m) => HiValue -> ExceptT HiError m B.ByteString -> ExceptT HiError m B.ByteString
    folder e r = r >>= \r' -> bytes8 e >>= \e' -> return $ e' `B.cons` r'

unaryEvaled (HiValueFunction HiFunUnpackBytes) x = applyUnary unpackBytes x
  where
    unpackBytes :: (HiMonad m) =>  HiValue -> ExceptT HiError m HiValue
    unpackBytes bs = (fromHiValue bs :: (HiMonad m) => ExceptT HiError m B.ByteString) >>= \bs' -> (return . HiValueList) (B.foldr folder Empty bs')
    folder ::  Word8 -> Seq HiValue -> Seq HiValue

    folder e r = (toHiValue . (fromIntegral :: Word8 -> Int)) e:<| r

unaryEvaled (HiValueFunction HiFunDecodeUtf8) x = applySafeUnary decode x
unaryEvaled (HiValueFunction HiFunEncodeUtf8) x = applySafeUnary TE.encodeUtf8 x

unaryEvaled (HiValueFunction HiFunZip) x = applySafeUnary comporess x
unaryEvaled (HiValueFunction HiFunUnzip) x = applySafeUnary decompress x

unaryEvaled (HiValueFunction HiFunSerialise) x = applySafeUnary (BL.toStrict . Seria.serialise :: HiValue -> B.ByteString) x
unaryEvaled (HiValueFunction HiFunDeserialise) x = applyUnary deserialise' x
  where
    deserialise' :: (HiMonad m) => B.ByteString -> ExceptT HiError m HiValue
    deserialise' bs = case Seria.deserialiseOrFail $ BL.fromStrict bs of
      Right v -> return v
      _ -> throwE HiErrorInvalidArgument

unaryEvaled (HiValueFunction HiFunRead) x = applySafeUnary HiActionRead x
unaryEvaled (HiValueFunction HiFunMkDir) x = applySafeUnary HiActionMkDir x
unaryEvaled (HiValueFunction HiFunChDir) x = applySafeUnary HiActionChDir x

unaryEvaled (HiValueFunction HiFunParseTime) x = applyUnary parseTime' x
  where
    parseTime' :: (HiMonad m) => String -> ExceptT HiError m HiValue
    parseTime' t = case readMaybe t of
      Nothing -> throwE HiErrorInvalidArgument
      Just t' -> return $ HiValueTime t'

unaryEvaled (HiValueFunction HiFunEcho) x = applySafeUnary HiActionEcho x
unaryEvaled (HiValueFunction HiFunKeys) x =
  applySafeUnary (fromList . M.keys :: M.Map HiValue HiValue -> Seq HiValue) x
unaryEvaled (HiValueFunction HiFunValues) x =
  applySafeUnary (fromList . M.elems :: M.Map HiValue HiValue -> Seq HiValue) x
unaryEvaled (HiValueFunction HiFunInvert) x = applySafeUnary (fmap HiValueList . mapMap) x
  where
    mapMap :: M.Map HiValue HiValue -> M.Map HiValue (Seq HiValue)
    mapMap = M.foldlWithKey updateMap M.empty
    updateMap :: M.Map HiValue (Seq HiValue) -> HiValue -> HiValue -> M.Map HiValue (Seq HiValue)
    updateMap b k v = M.insertWith (><) v (singleton k) b
unaryEvaled (HiValueFunction HiFunCount) x = applySafeUnary (listCount . map T.singleton) x
  <|> applySafeUnary (listCount . map toRational . B.unpack ) x
  <|> applySafeUnary (listCount . toList :: Seq HiValue -> M.Map HiValue HiValue) x
  where
    listCount :: (Valuable a) => [a] -> M.Map HiValue HiValue
    listCount l = fmap toHiValue $ M.fromListWith  (+) $ map (\x' -> (toHiValue x', 1 :: Int)) l


unaryEvaled (HiValueDict d) x = applySafeUnary (fromMaybe HiValueNull . (`M.lookup` d)) x

unaryEvaled _ _ = throwE HiErrorInvalidArgument

decode :: B.ByteString -> HiValue
decode bs = case TE.decodeUtf8' bs of
  Right t -> HiValueString t
  _ -> HiValueNull


list :: (HiMonad m) => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
list (HiValueFunction HiFunList) args = traverse evalExpr args F.<&> (HiValueList . fromList)
list (HiValueString s) args = hiRange s T.take T.drop index' T.length args
  where
    index' :: T.Text -> Int -> T.Text
    index' s' i = T.singleton $ T.index s' i
list (HiValueBytes bs) args = hiRange bs B.take B.drop index' B.length args
  where
    index' :: B.ByteString -> Int -> Rational
    index' bs' i = toRational $ B.index bs' i
list (HiValueList l) args = hiRange l S.take S.drop S.index S.length args
list _ _ = throwE HiErrorInvalidArgument


ternary :: (HiMonad m) => HiValue -> HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
ternary (HiValueFunction HiFunIf) cond a b =
  evalExpr cond >>= fromHiValue >>= \cond' -> evalExpr (if cond' then a else b)
ternary _ _ _ _ = throwE HiErrorInvalidArgument

type Take a = Int -> a -> a
type Drop a = Int -> a -> a
type Length a = a -> Int
type Get a b = a -> Int -> b

hiRange :: (HiMonad m, Valuable a, Valuable b) => a -> Take a -> Drop a -> Get a b -> Length a -> [HiExpr] -> ExceptT HiError m HiValue
hiRange s take' drop' get' length' args = traverse evalExpr args >>= \case
  [x] -> fromHiValue x >>= \x' -> get x'
  [HiValueNull, b] -> fromHiValue b >>= \b' -> rangeTo b'
  [a, HiValueNull] -> fromHiValue a >>= \a' -> rangeFrom a'
  [a, b] -> fromHiValue a >>= \a' -> fromHiValue b >>= \b' -> range a' b'
  _ -> throwE HiErrorArityMismatch
  where
    get :: (HiMonad m) => Int -> ExceptT HiError m HiValue
    get c = return $ if 0 <= c && c < l then toHiValue $ get' s c else HiValueNull
    range :: (HiMonad m) => Int -> Int -> ExceptT HiError m HiValue
    range = slice
    rangeTo :: (HiMonad m) => Int -> ExceptT HiError m HiValue
    rangeTo = slice 0
    rangeFrom :: (HiMonad m) => Int  -> ExceptT HiError m HiValue
    rangeFrom from = slice from l
    l = length' s
    slice :: (HiMonad m) => Int -> Int -> ExceptT HiError m HiValue
    slice x y =
         let
          findIndex a = if a >= 0 then a else l + a
          from = findIndex x
          to = findIndex y
          in return $ toHiValue $ if from > to
             then take' 0 s
              else take' (to - from) $ drop' from s


applyBinary :: (HiMonad m, Valuable a, Valuable b) => (a -> b -> ExceptT HiError m HiValue) -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyBinary f x1 x2 = fromHiValue x1 >>= \x1' -> fromHiValue x2 >>= \x2' -> f x1' x2'

applySafeBinary :: (HiMonad m, Valuable a, Valuable b, Valuable c) => (a -> b -> c) -> HiValue -> HiValue -> ExceptT HiError m HiValue
applySafeBinary f = applyBinary (\x y -> return $ toHiValue $ f x y)

applyUnary :: (HiMonad m, Valuable a) => (a -> ExceptT HiError m HiValue) -> HiValue -> ExceptT HiError m HiValue
applyUnary f arg = fromHiValue arg >>= \arg' -> f arg'

applySafeUnary :: (HiMonad m, Valuable a, Valuable b) => (a -> b) -> HiValue -> ExceptT HiError m HiValue
applySafeUnary f = applyUnary (return . toHiValue . f)

hiApply :: (HiMonad m) => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
hiApply f args = evalExpr f >>= \f' -> getArity f' >>= \arity -> case (arity, args) of
  (Unary, [a]) -> unary f' a
  (Binary, [a, b]) -> binary f' a b
  (Ternary, [a, b, c]) -> ternary f' a b c
  (List, l) -> list f' l
  _ -> throwE HiErrorArityMismatch

evalList :: (HiMonad m) => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
evalList exprs = case exprs of
  [] -> return []
  (e1, e2):t -> evalExpr e1 >>= \e1' -> evalExpr e2 >>= \e2' -> evalList t >>= \t' -> return ((e1',e2'):t')

runExpr :: (HiMonad m) => HiExpr -> ExceptT HiError m HiValue
runExpr e = evalExpr e >>= \case
  (HiValueAction a) -> ExceptT (Right <$> runAction a)
  _ -> throwE HiErrorInvalidArgument

evalExpr :: (HiMonad m) => HiExpr -> ExceptT HiError m HiValue
evalExpr = resolve
  where
    resolve :: (HiMonad m) => HiExpr -> ExceptT HiError m HiValue
    resolve (HiExprRun e) = runExpr e
    resolve (HiExprValue value)  = return value
    resolve (HiExprDict m) = HiValueDict . M.fromList <$> evalList m
    resolve (HiExprApply f args) = hiApply f args

eval :: (HiMonad m) => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr
