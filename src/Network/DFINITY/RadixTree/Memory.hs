module Network.DFINITY.RadixTree.Memory
   ( load
   , store
   ) where

import Control.DeepSeq (force)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Short (toShort)
import Data.LruCache as LRU (insert, lookup)
import Database.LevelDB (DB, defaultReadOptions, defaultWriteOptions, get, put)

import Network.DFINITY.RadixTree.Types

load :: MonadIO m => RadixCache -> DB -> ByteString -> m (Maybe ByteString)
load cache database key =
   case fst <$> LRU.lookup short cache of
      Nothing -> get database defaultReadOptions key
      Just value -> pure $ Just value
   where short = toShort key

store :: MonadIO m => RadixCache -> DB -> ByteString -> ByteString -> m RadixCache
store cache database key value = do
   put database defaultWriteOptions key value
   pure $ force $ insert short value cache
   where short = toShort key
