{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
module HW5.Base (
  HiFun(..)
  , HiExpr(..)
  , HiValue(..)
  , HiMonad (..)
  , HiAction(..)
  , HiError(..)
  , Valuable(..)
) where

import           Codec.Serialise            (Serialise)
import           Control.Monad.Trans.Except
import qualified Data.ByteString            as B
import           Data.Ratio                 (denominator, numerator)
import           Data.Sequence
import qualified Data.Text                  as T (Text, pack, unpack)
import           GHC.Generics               (Generic)
import Data.Time
import Data.Map
data HiFun =
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Generic)

instance Show HiFun where
  show HiFunMul = "mul"
  show HiFunDiv = "div"
  show HiFunAdd = "add"
  show HiFunSub = "sub"
  show HiFunAnd = "and"
  show HiFunOr = "or"
  show HiFunLessThan = "less-than"
  show HiFunGreaterThan = "greater-than"
  show HiFunEquals = "equals"
  show HiFunNotLessThan = "not-less-than"
  show HiFunNotGreaterThan = "not-less-than"
  show HiFunIf = "if"
  show HiFunNotEquals = "/="
  show HiFunLength = "length"
  show HiFunToUpper = "to-upper"
  show HiFunToLower = "to-lower"
  show HiFunReverse = "reverse"
  show HiFunTrim = "trim"
  show HiFunList = "list"
  show HiFunRange = "range"
  show HiFunFold = "fold"
  show HiFunPackBytes = "pack"
  show HiFunUnpackBytes = "unpack"
  show HiFunEncodeUtf8 = "encode-utf8"
  show HiFunDecodeUtf8 = "decode-utf8"
  show HiFunSerialise = "serialise"
  show HiFunDeserialise = "deserialise"
  show HiFunRead = "read"
  show HiFunWrite = "write"
  show HiFunMkDir = "mkdir"
  show HiFunChDir = "cd"
  show HiFunParseTime = "parse-time"
  show HiFunRand = "rand"
  show HiFunCount = "count"
  show HiFunKeys = "keys"
  show HiFunValues = "values"
  show HiFunInvert = "invert"
  show HiFunNot = "not"
  show HiFunZip = "zip"
  show HiFunUnzip = "unzip"
  show HiFunEcho = "echo"

instance Serialise HiFun

data HiValue =
  HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiExpr  =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)]
  | HiExprRun HiExpr
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiExpr

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

instance Semigroup HiError where
  e1 <> _ = e1

instance Monoid HiError where
  mempty = HiErrorInvalidArgument

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Eq, Show, Ord, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue


class Valuable a where
  toHiValue :: a -> HiValue
  fromHiValue :: (HiMonad m) => HiValue -> ExceptT HiError m a

instance Valuable (Map HiValue HiValue) where
  toHiValue = HiValueDict

  fromHiValue (HiValueDict m) = return m
  fromHiValue _ = throwE HiErrorInvalidArgument

instance Valuable HiAction where
  toHiValue = HiValueAction

  fromHiValue (HiValueAction a) = return a
  fromHiValue _                 = throwE HiErrorInvalidArgument

instance Valuable HiValue where
  toHiValue = id
  fromHiValue = return


instance Valuable T.Text where
  toHiValue = HiValueString

  fromHiValue (HiValueString t) = return t
  fromHiValue _                 = throwE HiErrorInvalidArgument

instance Valuable Rational where
  toHiValue = HiValueNumber

  fromHiValue (HiValueNumber r) = return r
  fromHiValue _                 = throwE HiErrorInvalidArgument

instance Valuable Int where
  toHiValue = HiValueNumber . toRational

  fromHiValue (HiValueNumber a) = if denominator a == 1 then return . fromInteger . numerator $ a else throwE HiErrorInvalidArgument
  fromHiValue _ = throwE HiErrorInvalidArgument

instance Valuable UTCTime where
  toHiValue = HiValueTime

  fromHiValue (HiValueTime t) = return t
  fromHiValue _ = throwE HiErrorInvalidArgument

instance Valuable String where
  toHiValue = HiValueString . T.pack

  fromHiValue (HiValueString t) = return $ T.unpack t
  fromHiValue _                 = throwE HiErrorInvalidArgument

instance Valuable (Seq HiValue) where
  toHiValue = HiValueList

  fromHiValue (HiValueList s) = return s
  fromHiValue _               = throwE HiErrorInvalidArgument

instance Valuable Bool where
  toHiValue = HiValueBool

  fromHiValue (HiValueBool s) = return s
  fromHiValue HiValueNull = return False
  fromHiValue _               = throwE HiErrorInvalidArgument

instance Valuable B.ByteString where
  toHiValue = HiValueBytes

  fromHiValue (HiValueBytes bs) = return bs
  fromHiValue _                 = throwE HiErrorInvalidArgument

instance Valuable HiFun where
  toHiValue = HiValueFunction

  fromHiValue (HiValueFunction f) = return f
  fromHiValue _                   = throwE HiErrorInvalidArgument
