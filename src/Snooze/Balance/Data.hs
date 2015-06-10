{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Balance.Data (
    BalanceTable (..)
  , BalanceEntry (..)
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
    balanceHost :: Text
  , balancePort :: Int
  } deriving (Eq, Show)

balanceTableList :: BalanceTable -> [BalanceEntry]
balanceTableList (BalanceTable h t) =
  h : t
