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
  , balanceTableStatic
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

-- | Define a static 'BalanceTable' that never changes
balanceTableStatic :: MonadIO m => BalanceTable -> m UpdatableBalanceTable
balanceTableStatic =
  liftIO . fmap updatableBalanceTable . newMVar

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
    unHost :: Text
  } deriving (Eq, Show, Ord)

newtype Weight = Weight {
    unWeight :: Int
  } deriving (Eq, Show, Ord)

newtype Port = Port {
    unPort :: Int
  } deriving (Eq, Show, Ord)
