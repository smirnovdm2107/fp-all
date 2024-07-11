module HW5.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString               as B (ByteString, unpack)
import           Data.Char                     (intToDigit)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Ratio
import           Data.Scientific
import           Data.Sequence
import qualified Data.Text                     as T (unpack)
import           HW5.Base                      (HiAction (..), HiValue (..))
import           Prettyprinter                 (Doc, pretty, viaShow)
import           Prettyprinter.Render.Terminal (AnsiStyle)

rationalToString :: Rational -> String
rationalToString val
  | r == 0 = show q
  | isNothing period = show res
  | q == 0 = (if frac > 0 then "" else "-") ++ absFracString
  | otherwise =
     show q ++ (if frac < 0 then "-" else "+") ++ absFracString
  where
    num = numerator val
    den = denominator val
    (q, r) = quotRem num den
    frac = (num - q * den)%den
    absFracString =  (show . abs . numerator $ frac) ++ "/" ++ (show . denominator) frac
    (res, period) = fromRationalRepetendUnlimited val

bytesToString :: B.ByteString -> String
bytesToString bs = "[#" ++ folded ++ " #]"
    where
      folded = foldl (\r e -> r ++ " " ++ (toHex . fromIntegral) e) "" (B.unpack bs)
      toHex n = [intToDigit (n `div` 16), intToDigit (n `mod` 16)]

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber val) = pretty $ rationalToString val
prettyValue (HiValueBool b) = pretty (if b then "true" else "false")
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = pretty ("\"" ++ T.unpack s ++ "\"")
prettyValue (HiValueList s) = case s of
  Empty -> pretty "[]"
  (h :<| r) -> pretty "[" <> prettyValue h <> go r <> pretty "]"
    where
      go :: Seq HiValue -> Doc AnsiStyle
      go = foldl (\r' e -> r' <> pretty ", " <> prettyValue e) (pretty "")

prettyValue (HiValueBytes bs) = pretty $ bytesToString bs

prettyValue (HiValueAction a) = pretty $ case a of
  HiActionRead filePath -> "read(" ++ show filePath ++ ")"
  HiActionWrite filePath bs -> "write(" ++ show filePath ++ ", " ++ bytesToString bs ++ ")"
  HiActionMkDir filePath -> "mkdir(" ++ show filePath ++ ")"
  HiActionChDir filePath -> "cd(" ++ show filePath ++ ")"
  HiActionCwd -> "cwd"
  HiActionNow -> "now"
  HiActionRand from to -> "rand(" ++ (rationalToString . toRational) from ++ ", " ++ (rationalToString . toRational) to ++ ")"
  HiActionEcho t -> "echo(" ++ quotes t ++ ")"
  where
    quotes :: (Show a) => a -> String
    quotes str = "\"" ++ show str ++ "\""

prettyValue (HiValueFunction fun) = viaShow fun
prettyValue (HiValueTime t) = pretty $ "parse-time(\"" ++ show t ++ "\")"

prettyValue (HiValueDict d) = case M.toList d of
  []  -> pretty "{}"
  p:t -> pretty "{ " <> showPair p <> showList' t <> pretty " }"
  where
    showList' :: [(HiValue, HiValue)] -> Doc AnsiStyle
    showList' = foldl (\e p -> e <> pretty ", " <> showPair p) (pretty "")
    showPair :: (HiValue, HiValue) -> Doc AnsiStyle
    showPair (k, v) = prettyValue k <> pretty ": " <> prettyValue v
