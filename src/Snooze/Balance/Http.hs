{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Balance.Http (
    httpBalanced
  , httpBalancedReq
  , httpBalancedT
  ) where

import           Control.Monad.Catch (catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString.Lazy as BSL

import           Network.HTTP.Client (Request, Response, HttpException)

import           P

import           Snooze.Balance.Control
import           Snooze.Balance.Core
import           Snooze.Balance.Data
import           Snooze.Core


httpBalanced ::
     MonadIO m
  => (Request -> Request)
  -> BalanceT m (Response BSL.ByteString)
httpBalanced req =
  ReaderT $ \c ->
    httpBalancedReq' (req . balanceRequest) c

httpBalancedReq ::
     MonadIO m
  => Request
  -> BalanceT m (Response BSL.ByteString)
httpBalancedReq r =
  ReaderT $ \c ->
    httpBalancedT c r

httpBalancedT ::
     MonadIO m
  => BalanceConfig
  -> Request
  -> EitherT BalanceError m (Response BSL.ByteString)
httpBalancedT c r =
    httpBalancedReq' (flip balanceRequest' r) c

httpBalancedReq' ::
     MonadIO m
  => (BalanceEntry -> Request)
  -> BalanceConfig
  -> EitherT BalanceError m (Response BSL.ByteString)
httpBalancedReq' r (BalanceConfig ubt rp mgr) = do
  bt <- getTable ubt
  let
    action _ b =
      fmap Right (httpGo mgr $ r b)
        `catch`
          \(e :: HttpException) -> pure $ Left e

  newEitherT . liftIO . fmap (\(m, e) -> maybeToRight (BalanceTimeout e) m) $
    randomRoundRobin' rp action (balanceTableList bt)
