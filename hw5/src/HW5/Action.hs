{-# LANGUAGE LambdaCase #-}

module HW5.Action(
  PermissionException(..)
  , HIO(..)
  , HiPermission(..)
) where

import           Control.Applicative (Alternative (..))
import           Control.Exception
import           Control.Monad
import           Data.ByteString     as BS
import           Data.Sequence       as S
import           Data.Set
import           Data.Text           as T
import           Data.Text.Encoding  as TE
import           Data.Text.IO        as TIO
import           Data.Time.Clock
import           HW5.Base
import           System.Directory
import           System.IO           ()
import           System.Random       (randomRIO)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Monad HIO where
  return a = HIO (\_ -> return a)
  hio >>= f = HIO $ \perms -> runHIO hio perms >>= \a -> runHIO (f a) perms

instance HiMonad HIO where
  runAction action = HIO (doAction action)


doAction :: HiAction -> Set HiPermission -> IO HiValue

doAction (HiActionRead filePath) perms = if member AllowRead perms
   then (decode' <$> BS.readFile filePath)
      <|> (mapList <$> listDirectory filePath)
    else throwIO (PermissionRequired AllowRead)
  where
    mapList :: [String] -> HiValue
    mapList = HiValueList . S.fromList . fmap toHiValue

    decode' :: BS.ByteString -> HiValue
    decode' bs = case  TE.decodeUtf8' bs of
       Right str -> HiValueString str
       _         -> HiValueBytes bs

doAction (HiActionChDir newDir) perms = if member AllowRead perms
    then setCurrentDirectory newDir >> return HiValueNull
      else throwIO (PermissionRequired AllowRead)

doAction HiActionCwd perms = if member AllowRead perms
    then HiValueString . T.pack <$> getCurrentDirectory
      else throwIO (PermissionRequired AllowRead)


doAction (HiActionWrite filePath bytes) perms = if member AllowWrite perms
    then BS.writeFile filePath bytes >> return HiValueNull
      else throwIO (PermissionRequired AllowWrite)

doAction (HiActionMkDir dir) perms = if member AllowWrite perms
    then createDirectory dir >> return HiValueNull
      else throwIO (PermissionRequired AllowWrite)

doAction HiActionNow perms = if member AllowTime perms
    then toHiValue <$> getCurrentTime
      else throwIO (PermissionRequired AllowTime)


doAction (HiActionRand from to) _ = toHiValue <$> randomRIO (from, to)

doAction (HiActionEcho t) perms = if member AllowWrite perms
    then TIO.putStrLn t >> return HiValueNull
      else throwIO (PermissionRequired AllowWrite)
