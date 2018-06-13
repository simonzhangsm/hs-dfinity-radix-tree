{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (foldM_, mzero)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON(..), Object, Value(..), eitherDecode)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy.Char8 as Lazy (readFile)
import Data.ByteString.Short (fromShort)
import Data.Default.Class (Default(..))
import Data.HashMap.Strict as Map (elems, lookup)
import Data.Text as Text (Text, drop)
import Data.Text.Encoding (encodeUtf8)
import System.Console.CmdArgs (Data, cmdArgs)

import Network.DFINITY.RadixTree

data Args
   = Args
   { database :: FilePath
   , file :: FilePath
   , test :: String
   } deriving Data

instance Default Args where
   def = Args "test/testdb" "test/tests.json" "*"

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
   op <- Map.lookup "op" object
   case op of
      "set" -> Insert <$> get "key" object <*> get "value" object
      "delete" -> Delete <$> get "key" object
      "get" -> Lookup <$> get "key" object <*> get "value" object
      "stateRoot" -> Merkleize <$> get "value" object
      _ -> Nothing

get :: Text -> Object -> Maybe ByteString
get key object = do
   value <- Map.lookup key object
   case value of
      String text -> Just $ fst $ decode $ encodeUtf8 $ Text.drop 2 text
      _ -> Nothing

step :: MonadIO m => RadixTree -> Op -> m RadixTree
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
         result <- lookupNonMerkleizedRadixTree key tree
         case result of
            Just (value', tree') | value == value' -> pure tree'
            Just (value', _) -> throw
               [ "Expecting value "
               , ", but received value "
               , " for key "
               ] [value, value', key]
            Nothing -> throw
               [ "Expecting value "
               , ", but received no value for key "
               ] [value, key]
      Merkleize value -> do
         (fromShort -> value', tree') <- merkleizeRadixTree tree
         if value == value'
         then pure tree'
         else throw
            [ "Expecting state root "
            , ", but received state root "
            ] [value, value']
      where
      throw err = fail . concat . zipWith mappend err . map show

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   contents <- Lazy.readFile file
   case eitherDecode contents of
      Left err -> fail err
      Right vctors -> runResourceT $ do
         tree <- createRadixTree 100 100 database Nothing
         if test == "*"
         then foldM_ step tree `mapM_` elems vctors
         else case Map.lookup test vctors of
            Nothing -> fail $ "unknown test vector: " ++ test
            Just ops -> foldM_ step tree $ concat [ops]
