{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Snooze.Balance.Data (
    BalanceTable (..)
  , UpdatableBalanceTable
  , BalanceEntry (..)
  , Host (..)
  , Port (..)
  , Weight (..)
  , balanceTableList
  , updatableBalanceTable
  , getTable
  ) where

import           Control.Concurrent
import           Control.Monad.IO.Class

import           Data.Text

import           P

newtype UpdatableBalanceTable = UpdatableBalanceTable {
    updatableBT :: MVar BalanceTable
  }

updatableBalanceTable :: MVar BalanceTable -> UpdatableBalanceTable
updatableBalanceTable m =
  UpdatableBalanceTable m

getTable :: MonadIO m => UpdatableBalanceTable -> m BalanceTable
getTable ubt =
  liftIO . readMVar $ updatableBT ubt

data BalanceTable =
  BalanceTable {
    balanceTableHead :: BalanceEntry
  , balanceTableTail :: [BalanceEntry]
  } deriving (Eq, Show)

data BalanceEntry =
  BalanceEntry {
    balanceHost :: Host
  , balancePort :: Port
  } deriving (Eq, Show)

balanceTableList :: BalanceTable -> [BalanceEntry]
balanceTableList (BalanceTable h t) =
  h : t

newtype Host = Host {
    host :: Text
  } deriving (Eq, Show, Ord)

newtype Weight = Weight {
    weight :: Int
  } deriving (Eq, Show, Ord)

newtype Port = Port {
    port :: Int
  } deriving (Eq, Show, Ord)
