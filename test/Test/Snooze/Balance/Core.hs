{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Snooze.Balance.Core where

import           Data.Text.Encoding as T

import           Network.HTTP.Client

import           P

import           Snooze.Url
import           Snooze.Balance.Core
import           Snooze.Balance.Data

import           System.IO

import           Test.Snooze.Arbitrary ()
import           Test.QuickCheck


prop_balanceRequest :: BalanceEntry -> Path -> Property
prop_balanceRequest be p =
  let requestToEntry r = BalanceEntry (T.decodeUtf8 $ host r) (port r)
  in Just be === fmap (requestToEntry . balanceRequest be . urlToRequest) (url "http://localhost:80" p)


return []
tests :: IO Bool
tests = $quickCheckAll
