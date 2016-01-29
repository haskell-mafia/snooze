{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Balance.Http (
    httpBalanced
  , httpBalanced'
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

import           Network.HTTP.Client

import           P

import           Snooze.Balance.Control
import           Snooze.Balance.Core
import           Snooze.Balance.Data
import           Snooze.Core

import           System.IO (IO)


httpBalanced ::
    (Applicative m, MonadIO m) =>
    (Request -> Request)
  -> BalanceT m (Response BSL.ByteString)
httpBalanced req = ReaderT $ \(BalanceConfig ubt rp mgr) -> EitherT $ do
  bt <- getTable ubt
  liftIO . fmap (\(m, e) -> maybeToRight (BalanceTimeout e) m) $ randomRoundRobin' rp
    (\_ b -> catch (fmap Right . httpGo mgr . req . balanceRequest $ b) (\(e :: HttpException) -> pure $ Left e))
    (balanceTableList bt)

httpBalanced' ::
    (Applicative m, MonadIO m) =>
    (Request -> Request)
  -> BalanceT m (Response (IO ByteString))
httpBalanced' req = ReaderT $ \(BalanceConfig ubt rp mgr) -> EitherT $ do
  bt <- getTable ubt
  liftIO . fmap (\(m, e) -> maybeToRight (BalanceTimeout e) m) $ randomRoundRobin' rp
    (\_ b -> catch (fmap Right . flip responseOpen mgr . httpSafe . req . balanceRequest $ b) (\(e :: HttpException) -> pure $ Left e))
    (balanceTableList bt)
