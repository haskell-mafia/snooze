{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Test.Snooze.Balance.Core where

import           Control.Concurrent
import           Control.Monad.Trans.Either

import           Data.List ((++))
import           Data.Text (Text)
import           Data.Text.Encoding as T

import           Disorder.Core.IO

import           Network.HTTP.Client

import           P hiding (snd, fst)

import           Snooze.Url
import           Snooze.Balance.Core
import           Snooze.Balance.Data hiding (port, host)

import           System.IO

import           Test.Snooze.Arbitrary ()
import           Test.QuickCheck
import           Twine.Snooze


prop_balanceRequest :: BalanceEntry -> Path -> Property
prop_balanceRequest be p =
  let requestToEntry r = BalanceEntry (Host (T.decodeUtf8 $ host r)) (Port (port r))
  in Just be === fmap (requestToEntry . balanceRequest be . urlToRequest) (url "http://localhost:80" p)

-- Should fail before forkIO.
prop_balance_initial_fail :: Text -> Property
prop_balance_initial_fail t = testIO $ do
  b <- runEitherT $ balance (milliseconds 100) (const . const $ pure ()) (pure $ Left t)
  r <- traverse getTable b
  pure $ r === (Left t)

-- Run a sequential list of errors
prop_balance_errors :: BalanceTable -> [BalanceTableResult] -> Property
prop_balance_errors b bs = testIO $ do
  let es = filter isError bs
  m <- newMVar b
  errors <- simulateAll m es
  pure $ errors === length es

prop_balance_errors_zero :: BalanceTable -> [BalanceTableResult] -> BalanceTable -> Property
prop_balance_errors_zero b bs b' = testIO $ do
  m <- newMVar b
  errors <- simulateAll m $ bs ++ [OkBalanceTable b']
  pure $ errors === 0

prop_balance_ok :: BalanceTable -> [BalanceTableResult] -> BalanceTable -> Property
prop_balance_ok b bs expected = testIO $ do
  m <- newMVar b
  void . simulateAll m $ bs ++ [OkBalanceTable expected]
  r <- readMVar m
  pure $ r === expected

simulateAll ::  (MVar BalanceTable) -> [BalanceTableResult] -> IO Int
simulateAll m trs = do
  e <- newMVar 0
  forM_ trs $ simulate' m e
  readMVar e

simulate ::  (MVar BalanceTable) -> BalanceTableResult -> IO Int
simulate m tr = do
  e <- newMVar 0
  simulate' m e tr
  readMVar e

simulate' ::  (MVar BalanceTable) -> (MVar Int) -> BalanceTableResult -> IO ()
simulate' m e tr = do
  tick m e (const . const $ pure ()) (pure $ runResult tr)

runResult :: BalanceTableResult -> Either Text BalanceTable
runResult = \case
  OkBalanceTable b ->
    Right b
  ErrBalanceTable t ->
    Left t

data BalanceTableResult =
  OkBalanceTable BalanceTable
  | ErrBalanceTable Text
  deriving (Eq, Show)

isError :: BalanceTableResult -> Bool
isError = \case
  OkBalanceTable _ -> False
  ErrBalanceTable _ -> True

instance Arbitrary BalanceTableResult where
  arbitrary = oneof [
      OkBalanceTable <$> arbitrary
    , ErrBalanceTable <$> arbitrary
    ]

return []
tests :: IO Bool
tests = $quickCheckAll
