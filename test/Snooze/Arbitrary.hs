{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Snooze.Arbitrary where

import           P

import           Snooze.Url

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary Url where
  arbitrary = url <$> arbitrary <*> arbitrary

instance Arbitrary Path where
  arbitrary = path <$> arbitrary
