{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import           Control.Concurrent.Classy     (MonadConc (readTVarConc), STM,
                                                atomically, modifyTVar)
import           Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar,
                                                writeTVar)

import           Data.Array.MArray             (getBounds, newArray, readArray,
                                                writeArray)

import           Data.Foldable                 (find)
import           Data.Hashable

import           Control.Monad
import           Data.Array.Base               (MArray)
import           Data.Bifunctor                (Bifunctor (first))

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: (MonadConc m) => m (CHT (STM m) k v)
newCHT = atomically $ do
    arr <- newArray (0, initCapacity - 1) []
    arr_var <- newTVar arr
    size_var <- newTVar 0
    return $ CHT arr_var size_var


getIndex :: (MArray a e m, Hashable k) => k -> a Int e -> m Int
getIndex key buckets = do
  (_, r) <- getBounds buckets
  return $ hash key `mod` (r + 1)

bucketSearch :: Eq k => Bucket k v -> k -> Maybe v
bucketSearch bucket k = snd <$> find (\(k', _) -> k' == k) bucket

getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT k cht = atomically $ do
  buckets <- readTVar $ chtBuckets cht
  index <- getIndex k buckets
  bucket <- readArray buckets index
  return $ bucketSearch bucket k


type IsSizeIncreaed = Bool
putBucket :: Eq k => Bucket k v -> k -> v -> (Bucket k v, IsSizeIncreaed)
putBucket [] k v = ([(k, v)], True)
putBucket ((k', v'):rest) k v = if k' == k then ((k, v):rest, False) else first ((k',v') :) (putBucket rest k v)

putCHT :: (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value cht = do
  ensureCapasity cht
  unsafePutCHT key value cht

unsafePutCHT :: (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
unsafePutCHT key value cht = atomically $ do
  let sizeVar = chtSize cht
  buckets <- readTVar $ chtBuckets cht
  index <- getIndex key buckets
  bucket <- readArray buckets index
  let (newBucket, isSizeIncreaed) = putBucket bucket key value
  writeArray buckets index newBucket
  when isSizeIncreaed $ modifyTVar sizeVar (+1)


ensureCapasity :: (MonadConc m, Hashable k) => CHT (STM m) k v -> m ()
ensureCapasity cht = atomically $ do
  size <- readTVar $ chtSize cht
  let bucketsVar = chtBuckets cht
  buckets <- readTVar bucketsVar
  (l, r) <- getBounds buckets
  let capacity = r - l + 1
  when (fromIntegral size >= fromIntegral capacity * loadFactor) $ do
    let newCapacity = capacity * 2
    newBuckets <- newArray (0, newCapacity - 1) []
    forM_ [l .. r] $ \i -> do
      oldBucket <- readArray buckets i
      updateArray newBuckets newCapacity oldBucket
    writeTVar bucketsVar newBuckets
  where
    updateArray _ _ [] = return ()
    updateArray newBuckets newCapacity (p@(k,_):rest) = do
      let index = hash k `mod` newCapacity
      bucket <- readArray newBuckets index
      writeArray newBuckets index (p:bucket)
      updateArray newBuckets newCapacity rest

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = readTVarConc $ chtSize cht
