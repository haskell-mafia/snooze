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
balanceTableToText (BalanceTable h t) =
  T.unlines $ fmap balanceEntryToText (h : t)

balanceEntryToText :: BalanceEntry -> Text
balanceEntryToText (BalanceEntry h p) =
  T.intercalate " " [h, T.pack $ show p]

balanceTableFromText :: Text -> Either String BalanceTable
balanceTableFromText =
  parseOnly balanceTableParser

balanceTableParser :: Parser BalanceTable
balanceTableParser = do
  e <- balanceEntryParser
  _ <- char '\n'
  f <- balanceEntryParser `sepBy` (char '\n')
  pure $ BalanceTable e f

balanceEntryParser :: Parser BalanceEntry
balanceEntryParser = do
  h <- T.pack <$> manyTill anyChar (char ' ')
  p <- many' digit >>= fromMaybeM (fail "Invalid port") . readMaybe
  pure $ BalanceEntry h p
