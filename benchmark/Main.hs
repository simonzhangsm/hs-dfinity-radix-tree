{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-cse #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Builder (word32BE, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class (Default(..))
import Data.Word (Word32)
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
   -> Word32
   -> m RadixTree
step action tree i = do
   tree' <- action tree key
   if mod i 1000 == 0
   then snd <$> merkleizeRadixTree tree'
   else pure tree'
   where key = hashlazy $ toLazyByteString $ word32BE i

foldInsert
   :: MonadIO m
   => RadixTree
   -> [Word32]
   -> m RadixTree
foldInsert = foldM $ step $ flip $ \ key -> insertRadixTree key key

foldDelete
   :: MonadIO m
   => RadixTree
   -> [Word32]
   -> m RadixTree
foldDelete = foldM $ step $ flip deleteRadixTree

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   runResourceT $ do
      tree <- createRadixTree 262144 2048 database Nothing
      tree' <- foldInsert tree [1..100000]
      tree'' <- foldDelete tree' [1..100000]
      liftIO $ print $ isEmptyRadixTree tree''
