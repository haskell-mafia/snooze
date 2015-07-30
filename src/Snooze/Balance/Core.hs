{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Balance.Core (
    balanceRequest
  , balance
  , tick
  ) where

import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import           Data.Text.Encoding as TL

import           Network.HTTP.Client

import           P

import           Snooze.Balance.Data hiding (host, port)

import           System.IO

import           Twine.Snooze

balanceRequest :: BalanceEntry -> Request -> Request
balanceRequest (BalanceEntry (Host h) (Port p)) r = r {
    host = TL.encodeUtf8 h
  , port = p
  }

balance :: (Applicative m, MonadIO m) =>
              Duration
           -> (Int -> e -> IO ())
           -> IO (Either e BalanceTable)
           -> EitherT e m UpdatableBalanceTable
balance d e f = do
  z <- liftIO $ f
  m <- either left (liftIO . newMVar) z
  _ <- liftIO . forkIO $ worker m d e f
  right $ updatableBalanceTable m

worker :: (MVar BalanceTable) -> Duration -> (Int -> e -> IO ()) -> IO (Either e BalanceTable) -> IO ()
worker m d eh f = do
  em <- newMVar 0
  forever $ do
    snooze d
    tick m em eh f

tick :: (MVar BalanceTable) -> (MVar Int) -> (Int -> e -> IO ()) -> IO (Either e BalanceTable) -> IO ()
tick m d eh f = do
  r <- f
  case r of
    Left e -> do
      i <- modifyMVar d (\i -> pure (i + 1, i))
      eh i e
    Right bt -> do
      void $ swapMVar d 0
      void $ swapMVar m bt
