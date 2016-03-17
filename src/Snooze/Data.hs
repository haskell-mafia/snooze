{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Data (
    Service (..)
  , Registry (..)
  , Entry (..)
  , ContentType (..)
  ) where


import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Map.Strict (Map)

import           P

import           Snooze.Balance.Data (UpdatableBalanceTable)

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
