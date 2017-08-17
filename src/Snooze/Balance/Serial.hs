{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Balance.Serial (
    balanceTableToText
  , balanceTableFromText
  , balanceTableParser
  , balanceEntryParser
  ) where

import           Data.Attoparsec.Text
import           Data.String (String)
import           Data.Text as T

import           Snooze.Balance.Data

import           P


balanceTableToText :: BalanceTable -> Text
balanceTableToText =
  T.unlines . fmap balanceEntryToText . balanceTableList

balanceEntryToText :: BalanceEntry -> Text
balanceEntryToText (BalanceEntry (Host h) (Port p)) =
  T.intercalate " " [h, T.pack $ show p]

balanceTableFromText :: Text -> Either String BalanceTable
balanceTableFromText =
  parseOnly balanceTableParser

balanceTableParser :: Parser BalanceTable
balanceTableParser =
  BalanceTable <$> balanceEntryParser `sepBy` (char '\n')

balanceEntryParser :: Parser BalanceEntry
balanceEntryParser = do
  h <- T.pack <$> manyTill anyChar (char ' ')
  p <- many' digit >>= fromMaybeM (fail "Invalid port") . readMaybe
  pure $ BalanceEntry (Host h) (Port p)
