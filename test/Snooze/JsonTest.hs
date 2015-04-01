{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.JsonTest where

import           Data.String (String)
import           Data.Text as T

import           P

import           Snooze.Arbitrary ()
import           Snooze.Core as C
import           Snooze.CoreTest
import           Snooze.Json
import           Snooze.Server
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


-- This is bullshit - you can't create a Response directly :(
prop_decodeResponse :: [String] -> Path -> Property
prop_decodeResponse j p = monadicIO $ do
  r <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.json j) $ \u ->
       C.get u []

  stop $ decodeResponse r === Right j

prop_decodeResponse_fail :: [String] -> Path -> Property
prop_decodeResponse_fail j p = monadicIO $ do
  r <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.json j) $ \u ->
       C.get u []

  stop $ isLeft ((decodeResponse r) :: Either Text String) === True


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
