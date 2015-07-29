{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Balance.Data (
    BalanceTable (..)
  , BalanceEntry (..)
  , Host (..)
  , Port (..)
  , Weight (..)
  , balanceTableList
  ) where

import           Data.Text

import           P


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
