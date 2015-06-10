{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Snooze.Arbitrary where

import           Disorder.Corpus

import           P

import           Snooze.Balance.Data
import           Snooze.Url

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary BalanceTable where
  arbitrary = BalanceTable
    <$> arbitrary
    <*> arbitrary

instance Arbitrary BalanceEntry where
  arbitrary = BalanceEntry
    <$> elements muppets
    <*> fmap getPositive arbitrary

instance Arbitrary Path where
  arbitrary = path <$> arbitrary
