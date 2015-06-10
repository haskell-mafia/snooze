{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Snooze.Balance.Serial where

import           P

import           Snooze.Balance.Serial

import           System.IO

import           Test.Snooze.Arbitrary ()
import           Test.QuickCheck


prop_balance bt =
  balanceTableFromText (balanceTableToText (bt)) === Right bt


return []
tests :: IO Bool
tests = $quickCheckAll
