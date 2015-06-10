{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Snooze.Core where

import           Data.ByteString.Lazy as BSL

import           Disorder.Core.IO

import           Network.HTTP.Client as HC
import           Network.HTTP.Types.Status

import           P

import           Snooze.Balance
import           Snooze.Core as C
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


prop_get :: BSL.ByteString -> Path -> Property
prop_get bs p = monadicIO $ do
  x <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.raw bs) $ \u ->
       C.get u []

  stop $ responseBody x === bs

prop_get_status :: Path -> Property
prop_get_status p = monadicIO $ do
  x <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.status status400) $ \u ->
      C.get u []

  stop $ responseStatus x === status400

prop_post :: BSL.ByteString -> Path -> Property
prop_post bs p = monadicIO $ do
  x <- run . withServer' p
    (S.post (pathRoutePattern p) $ S.body >>= S.raw) $ \u ->
      C.post u [] bs

  stop $ responseBody x === bs

prop_post_status :: BSL.ByteString -> Path -> Property
prop_post_status bs p = monadicIO $ do
  x <- run . withServer' p
    (S.post (pathRoutePattern p) $ S.status status400) $ \u ->
      C.post u [] bs

  stop $ responseStatus x === status400

prop_delete :: BSL.ByteString -> Path -> Property
prop_delete bs p = monadicIO $ do
  x <- run . withServer' p
    (S.delete (pathRoutePattern p) $ S.raw bs) $ \u ->
      C.delete u []

  stop $ responseBody x === bs

prop_delete_status :: Path -> Property
prop_delete_status p = monadicIO $ do
  x <- run . withServer' p
    (S.delete (pathRoutePattern p) $ S.status status400) $ \u ->
      C.delete u []

  stop $ responseStatus x === status400

prop_httpBalanced = once . testIO $ do
  let p = Snooze.Url.path []
  let get' = S.get (pathRoutePattern p) $ S.status status500
  withServer' p get' $ \u1 -> do
  withServer' p get' $ \u2 -> do
    let bt = BalanceTable
           (BalanceEntry (Host "localhost") (Port . HC.port $ urlToRequest u1))
          [ BalanceEntry (Host "localhost") (Port . HC.port $ urlToRequest u2)
          , BalanceEntry (Host "localhost") (Port 81)
          , BalanceEntry (Host "localhost") (Port 444)
          ]
    x <- C.httpBalanced bt (limitRetries 3) (urlToRequest u1) { HC.port = 1 }
    pure $ fmap responseStatus x === Just status500


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
