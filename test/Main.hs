{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Arrow (first)
import Control.Monad (foldM_, mzero, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Aeson (FromJSON(..), Object, Value(..), eitherDecode)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy.Char8 as Lazy (readFile)
import Data.ByteString.Short (fromShort)
import Data.Default.Class (Default(..))
import Data.HashMap.Strict as HashMap (elems, lookup)
import Data.Map.Strict (Map, empty)
import Data.Text as Text (Text, drop)
import Data.Text.Encoding (encodeUtf8)
import System.Console.CmdArgs (Data, cmdArgs)

import Network.DFINITY.RadixTree

data Args
   = Args
   { json :: FilePath
   , test :: String
   } deriving Data

instance Default Args where
   def = Args "test/tests.json" "*"

data Op
   = Insert ByteString ByteString
   | Delete ByteString
   | Lookup ByteString ByteString
   | Merkleize ByteString

instance FromJSON Op where
   parseJSON = \ case
      Object object -> maybe mzero pure $ parse object
      _ -> mzero

instance Show Op where
   show = \ case
      Insert key value -> "Insert" ++ pretty key ++ pretty value
      Delete key -> "Delete" ++ pretty key
      Lookup key value -> "Lookup" ++ pretty key ++ pretty value
      Merkleize value -> "Merkleize" ++ pretty value
      where pretty = mappend " 0x" . unpack . encode

parse :: Object -> Maybe Op
parse object = do
   op <- HashMap.lookup "op" object
   case op of
      "set" -> Insert <$> get "key" object <*> get "value" object
      "delete" -> Delete <$> get "key" object
      "get" -> Lookup <$> get "key" object <*> get "value" object
      "stateRoot" -> Merkleize <$> get "value" object
      _ -> Nothing

get :: Text -> Object -> Maybe ByteString
get key object = do
   value <- HashMap.lookup key object
   case value of
      String text -> Just $ fst $ decode $ encodeUtf8 $ Text.drop 2 text
      _ -> Nothing

step :: RadixTree () -> Op -> StateT (Map ByteString ByteString) IO (RadixTree ())
step tree op = do
   liftIO $ print op
   case op of
      Insert key value -> do
         tree' <- insertRadixTree key value tree
         printNonMerkleizedRadixTree tree'
         pure tree'
      Delete key -> do
         tree' <- deleteRadixTree key tree
         printNonMerkleizedRadixTree tree'
         pure tree'
      Lookup key value -> do
         result <- lookupRadixTree key tree
         case result of
            Just (value', tree') | value == value' -> pure tree'
            Just (value', _) -> throw
               ["Expecting value ", ", but received value ", " for key "]
               [value, value', key]
            Nothing -> throw
               ["Expecting value ", ", but received no value for key "]
               [value, key]
      Merkleize value -> do
         (value', tree') <- first fromShort <$> merkleizeRadixTree tree
         if value == value'
         then pure tree'
         else throw
            ["Expecting state root ", ", but received state root "]
            [value, value']
   where
   throw err = fail . concat . zipWith mappend err . map show

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   contents <- Lazy.readFile json
   case eitherDecode contents of
      Left err -> fail err
      Right vctors -> void $ flip runStateT empty $ do
         tree <- createRadixTree 262144 2048 Nothing ()
         if test == "*"
         then foldM_ step tree `mapM_` elems vctors
         else case HashMap.lookup test vctors of
            Nothing -> fail $ "Unknown test vector " ++ show test
            Just ops -> foldM_ step tree $ concat [ops]
