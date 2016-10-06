{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Snooze.Core where

import           Data.ByteString.Lazy as BSL

import           Network.HTTP.Client

import           P

import           Snooze.Core as C

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.IO.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


prop_get :: BSL.ByteString -> Property
prop_get bs = monadicIO $ do
  x <- run $ withServer (S.get "/" $ S.raw bs) httpGo'

  stop $ responseBody x === bs



return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
