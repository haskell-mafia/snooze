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
  ) where


import           Control.Retry (RetryPolicy)

import           Data.ByteString (ByteString)
import           Data.String (IsString)
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
