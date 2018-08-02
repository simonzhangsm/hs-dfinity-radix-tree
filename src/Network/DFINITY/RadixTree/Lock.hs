{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Lock
   ( withReadLock
   , withWriteLock
   ) where

import Control.Concurrent.ReadWriteLock (RWLock, acquireRead, acquireWrite, releaseRead, releaseWrite)
import Control.Monad.Trans.Resource (MonadResource, allocate, release)

withReadLock :: MonadResource m => RWLock -> m a -> m a
withReadLock lock action = do
   key <- fst <$> allocate acquireLock releaseLock
   result <- action
   release key
   pure result
   where
   acquireLock = acquireRead lock
   releaseLock = const $ releaseRead lock

withWriteLock :: MonadResource m => RWLock -> m a -> m a
withWriteLock lock action = do
   key <- fst <$> allocate acquireLock releaseLock
   result <- action
   release key
   pure result
   where
   acquireLock = acquireWrite lock
   releaseLock = const $ releaseWrite lock
