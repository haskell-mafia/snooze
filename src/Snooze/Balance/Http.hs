{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Balance.Http (
    httpBalanced
  , httpBalancedReq
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Data.ByteString.Lazy as BSL

import           Network.HTTP.Client

import           P

import           Snooze.Balance.Control
import           Snooze.Balance.Core
import           Snooze.Balance.Data
import           Snooze.Core


httpBalanced ::
    MonadIO m =>
    (Request -> Request)
  -> BalanceT m (Response BSL.ByteString)
httpBalanced req =
  httpBalancedReq' (req . balanceRequest)

httpBalancedReq ::
     MonadIO m
  => Request
  -> BalanceT m (Response BSL.ByteString)
httpBalancedReq r =
  httpBalancedReq' $ flip balanceRequest' r

httpBalancedReq' ::
     MonadIO m
  => (BalanceEntry -> Request)
  -> BalanceT m (Response BSL.ByteString)
httpBalancedReq' r = ReaderT $ \(BalanceConfig ubt rp mgr) -> EitherT $ do
  bt <- getTable ubt
  liftIO . fmap (\(m, e) -> maybeToRight (BalanceTimeout e) m) $ randomRoundRobin' rp
    (\_ b -> catch (fmap Right . httpGo mgr $ r b) (\(e :: HttpException) -> pure $ Left e))
    (balanceTableList bt)
