{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Snooze.Json where

import           Data.String (String)
import           Data.Text as T

import           P

import           Snooze.Core as C
import           Snooze.Json

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.IO.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


-- This is bullshit - you can't create a Response directly :(
prop_decodeResponse :: [String] -> Property
prop_decodeResponse j = monadicIO $ do
  r <- run $ withServer (S.get "/" $ S.json j) C.httpGo'

  stop $ decodeResponse r === Right j

prop_decodeResponse_fail :: [String] -> Property
prop_decodeResponse_fail j = monadicIO $ do
  r <- run $ withServer (S.get "/" $ S.json j) C.httpGo'

  stop $ isLeft ((decodeResponse r) :: Either Text String) === True


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
