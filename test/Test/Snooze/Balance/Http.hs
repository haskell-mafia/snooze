{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Snooze.Balance.Http where

import           Disorder.Core.IO

import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import           P

import           Snooze.Balance
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_httpBalanced = once . testIO $ do
  let p = Snooze.Url.path []
  let get' = S.get (pathRoutePattern p) $ S.status status500
  m <- newManager defaultManagerSettings
  withServer' p get' $ \u1 -> do
  withServer' p get' $ \u2 -> do
    let bt = BalanceTable
           (BalanceEntry (Host "localhost") (Port . port $ urlToRequest u1))
          [ BalanceEntry (Host "localhost") (Port . port $ urlToRequest u2)
          , BalanceEntry (Host "localhost") (Port 81)
          , BalanceEntry (Host "localhost") (Port 444)
          ]
    ubt <- balanceTableStatic bt
    x <- runBalanceT (BalanceConfig ubt (limitRetries 3) m) $ httpBalanced id
    pure $ fmap responseStatus (rightToMaybe x) === Just status500


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
