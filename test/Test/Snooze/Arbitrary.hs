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

instance Arbitrary BalanceEntry where
  arbitrary = BalanceEntry
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Host where
  arbitrary = Host
    <$> elements muppets

instance Arbitrary Port where
  arbitrary = Port
    <$> fmap getPositive arbitrary

instance Arbitrary Path where
  arbitrary = path <$> arbitrary
