{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-cse #-}

module Main where

import Control.Arrow ((>>>))
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Builder (word32BE, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class (Default(..))
import Data.Maybe (isJust)
import Data.Word (Word32)
import Data.Data (Data)
import System.Console.CmdArgs (cmdArgs)
import System.IO.Temp (withSystemTempDirectory)

import qualified Criterion

import qualified Database.LevelDB as LevelDB
import qualified Database.LMDB.Simple as LMDB

import Network.DFINITY.RadixTree

data Args
  = Args
    { lmdb             :: !Bool
    , lmdb_database    :: !(Maybe FilePath)
    , leveldb          :: !Bool
    , leveldb_database :: !(Maybe FilePath)
    }
  deriving (Data)

instance Default Args where
   def = Args False Nothing False Nothing

isDivisibleBy :: (Integral i) => i -> i -> Bool
isDivisibleBy a b = (a `mod` b) == 0

foldN
   :: (RadixDatabase m db, Monad n)
   => Int
   -> (forall a. m a -> n a)
   -> (RadixTree db -> ByteString -> m (RadixTree db))
   -> RadixTree db
   -> [Word32]
   -> n (RadixTree db)
foldN _ _      _      tree [] = pure tree
foldN n commit action tree xs = do
  let toKey = hashlazy . toLazyByteString . word32BE
  tree' <- commit (foldM (\t -> action t . toKey) tree (take n xs)
                   >>= (merkleizeRadixTree >>> fmap snd))
  foldN n commit action tree' (drop n xs)

foldInsert
  :: (RadixDatabase m db, Monad n)
  => (forall a. m a -> n a)
  -> RadixTree db
  -> [Word32]
  -> n (RadixTree db)
foldInsert commit
  = foldN 2000 commit (\db key -> insertRadixTree key key db)

foldDelete
  :: (RadixDatabase m db, Monad n)
  => (forall a. m a -> n a)
  -> RadixTree db
  -> [Word32]
  -> n (RadixTree db)
foldDelete commit
  = foldN 2000 commit (\db key -> deleteRadixTree key db)

withDatabasePath :: Maybe FilePath -> String -> (FilePath -> IO a) -> IO a
withDatabasePath Nothing suffix callback = do
  let name = "dfinity-radix-tree-benchmark-" ++ suffix
  withSystemTempDirectory name callback
withDatabasePath (Just fp) _ callback = do
  callback fp

benchmarkLevelDB :: Args -> IO ()
benchmarkLevelDB args = do
  withDatabasePath (leveldb_database args) "leveldb" $ \dbPath -> do
    let options = def { LevelDB.createIfMissing = True }
    runResourceT $ do
      handle <- LevelDB.open dbPath options
      tree1 <- createRadixTree 262144 2048 Nothing handle
      tree2 <- foldInsert id tree1 [1 .. 100000]
      tree3 <- foldDelete id tree2 [1 .. 100000]
      liftIO $ print $ isEmptyRadixTree tree3

benchmarkLMDB :: Args -> IO ()
benchmarkLMDB args = do
  withDatabasePath (lmdb_database args) "lmdb" $ \dbPath -> do
    let limits = LMDB.defaultLimits { LMDB.mapSize = (1024 * 1024 * 512) }
    env <- LMDB.openReadWriteEnvironment dbPath limits
    db <- LMDB.readOnlyTransaction env (LMDB.getDatabase Nothing)
          :: IO (LMDB.Database ByteString ByteString)
    let runRW = LMDB.readWriteTransaction env
    tree1 <- runRW $ createRadixTree 262144 2048 Nothing db
    tree2 <- foldInsert runRW tree1 [1 .. 100000]
    tree3 <- foldDelete runRW tree2 [1 .. 100000]
    liftIO (print (isEmptyRadixTree tree3))

main :: IO ()
main = do
  args <- cmdArgs def
  when (not (lmdb args) && isJust (lmdb_database args)) $ do
    putStrLn "[WARNING] --lmdb-database specified but not --lmdb"
  when (not (leveldb args) && isJust (leveldb_database args)) $ do
    putStrLn "[WARNING] --leveldb-database specified but not --leveldb"
  when (not (lmdb args) && not (leveldb args)) $ do
    putStrLn "[NOTE] neither --lmdb nor --leveldb was specified, quitting"
  when (leveldb args) $ do
    putStrLn "[NOTE] benchmarking LevelDB"
    benchmarkLevelDB args
  when (lmdb args) $ do
    putStrLn "[NOTE] benchmarking LMDB"
    benchmarkLMDB args
