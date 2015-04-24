{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Snooze.Arbitrary where

import           P

import           Snooze.Url

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary Path where
  arbitrary = path <$> arbitrary
