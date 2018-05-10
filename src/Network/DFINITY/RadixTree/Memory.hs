module Network.DFINITY.RadixTree.Memory
   ( load
   , store
   ) where

import Control.DeepSeq (force)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.LruCache as LRU (LruCache, insert, lookup)
import Database.LevelDB (DB, defaultReadOptions, defaultWriteOptions, get, put)

load
   :: LruCache ShortByteString ByteString
   -> DB
   -> ByteString
   -> IO (Maybe ByteString)
load cache database key =
   case fst <$> LRU.lookup short cache of
      Nothing -> get database defaultReadOptions key
      Just value -> pure $ Just value
   where short = toShort key

store
   :: LruCache ShortByteString ByteString
   -> DB
   -> ByteString
   -> ByteString
   -> IO (LruCache ShortByteString ByteString)
store cache database key value = do
   put database defaultWriteOptions key value
   pure $ force $ insert short value cache
   where short = toShort key
