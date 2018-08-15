{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-cse #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Builder (word32BE, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class (Default(..))
import Data.Word (Word32)
import Database.LevelDB (DB, Options(..), open)
import System.Console.CmdArgs (Data, cmdArgs)

import Network.DFINITY.RadixTree

data Args
   = Args
   { database :: FilePath
   } deriving Data

instance Default Args where
   def = Args "database"

step
   :: MonadResource m
   => (RadixTree DB -> ByteString -> m (RadixTree DB))
   -> RadixTree DB
   -> Word32
   -> m (RadixTree DB)
step action tree i = do
   tree' <- action tree key
   if mod i 1000 == 0
   then snd <$> merkleizeRadixTree tree'
   else pure tree'
   where key = hashlazy $ toLazyByteString $ word32BE i

foldInsert
   :: MonadResource m
   => RadixTree DB
   -> [Word32]
   -> m (RadixTree DB)
foldInsert = foldM $ step $ flip $ \ key -> insertRadixTree key key

foldDelete
   :: MonadResource m
   => RadixTree DB
   -> [Word32]
   -> m (RadixTree DB)
foldDelete = foldM $ step $ flip deleteRadixTree

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   runResourceT $ do
      let
      handle <- open database options
      tree <- createRadixTree 262144 2048 Nothing handle
      tree' <- foldInsert tree [1..100000]
      tree'' <- foldDelete tree' [1..100000]
      liftIO $ print $ isEmptyRadixTree tree''
      where options = def {createIfMissing = True}
