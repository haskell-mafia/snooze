{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Snooze.Data (
    Component (..)
  , Query (..)
  , Service (..)
  , Registry (..)
  , Entry (..)
  , ContentType (..)
  , SnoozeURI (..)
  , parseURI
  ) where


import           Control.Retry (RetryPolicy)

import           Data.ByteString (ByteString)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map.Strict (Map)

import           P

import           Snooze.Balance.Data (UpdatableBalanceTable)

import qualified Network.URI as U

newtype Component =
  Component {
      component :: Text
    } deriving (Eq, Show, Ord, IsString)

data Query =
  Query {
      queryKey :: Text
    , queryValue :: Text
    } deriving (Eq, Show)

newtype Service =
  Service {
      serviceName :: Text
    } deriving (Eq, Show, Ord)

newtype ContentType =
  ContentType {
      contentType :: ByteString
    } deriving (Eq, Show, Ord)

data Entry =
  Entry {
      entryContentType :: ContentType
    , entryBalanceTable :: UpdatableBalanceTable
    , entryRetryPolicy :: RetryPolicy
    }

newtype Registry =
  Registry {
      mappings :: Map Service Entry
    }

newtype Snooze m a =
  Snooze {
      runSnooze :: ReaderT Registry m a
    }

data SnoozeURI =
    DirectHTTP !U.URI
  | DirectHTTPS !U.URI
  | SnoozeHTTP !U.URI
  | SnoozeHTTPS !U.URI

parseURI :: Text -> Maybe SnoozeURI
parseURI s =
  case U.parseURI . T.unpack $ s of
    Nothing ->
      Nothing
    Just v ->
      case U.uriScheme v of
        "http" ->
          Just $ DirectHTTP v
        "https" ->
          Just $ DirectHTTPS v
        "http+snooze" ->
          Just $ SnoozeHTTP v
        "https+snooze" ->
          Just $ SnoozeHTTPS v
        _ ->
          Nothing
