{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Snooze.Balance.Http where

import           Data.Default (def)

import           Disorder.Core.IO

import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import           P

import           Snooze.Balance

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.IO.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_httpBalanced = once . testIO $ do
  let get' = S.get "/" $ S.status status500
  m <- newManager defaultManagerSettings
  withServer get' $ \u1 -> do
  withServer get' $ \u2 -> do
    ubt <- balanceTableStatic $ serverBT u1 u2
    x <- runBalanceT (BalanceConfig ubt (limitRetries 3) m) $ httpBalanced id
    pure $ fmap responseStatus (rightToMaybe x) === Just status500

prop_httpBalancedReq = once . testIO $ do
  let get' = S.get "/" $ S.status status500
  m <- newManager defaultManagerSettings
  withServer get' $ \u1 -> do
  withServer get' $ \u2 -> do
    ubt <- balanceTableStatic $ serverBT u1 u2
    x <- runBalanceT (BalanceConfig ubt (limitRetries 3) m) $ httpBalancedReq def
    pure $ fmap responseStatus (rightToMaybe x) === Just status500

serverBT p1 p2 =
  BalanceTable
    (BalanceEntry (Host "localhost") (Port . port $ p1))
    [ BalanceEntry (Host "localhost") (Port . port $ p2)
    , BalanceEntry (Host "localhost") (Port 81)
    , BalanceEntry (Host "localhost") (Port 444)
    ]

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
