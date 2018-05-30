{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Default.Class (Default(..))
import System.Console.CmdArgs (Data, cmdArgs)

import Network.DFINITY.RadixTree

data Args
   = Args
   { database :: FilePath
   } deriving Data

instance Default Args where
   def = Args "benchmark/benchmarkdb"

step
   :: MonadIO m
   => (RadixTree -> ByteString -> m RadixTree)
   -> RadixTree
   -> Int
   -> m RadixTree
step action tree i =
   if mod i 1000 == 0
   then merkleizeRadixTree tree >>= flip action key . snd
   else action tree key
   where key = hash $ pack $ show i

stepInsert
   :: MonadIO m
   => RadixTree
   -> Int
   -> m RadixTree
stepInsert = step $ flip $ \ key -> insertRadixTree key key

stepDelete
   :: MonadIO m
   => RadixTree
   -> Int
   -> m RadixTree
stepDelete = step $ flip deleteRadixTree

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   runResourceT $ do
      tree <- createRadixTree 65536 2048 database Nothing
      tree' <- foldM stepInsert tree keys
      tree'' <- foldM stepDelete tree' keys
      liftIO $ print $ isEmptyRadixTree tree''
      where keys = [1..100000]
