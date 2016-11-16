{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Snooze.Balance.Core (
    module Control.Retry
  , newRequest
  , balanceRequest
  , balanceRequest'
  , balance
  , tick
  , randomRoundRobin'
  , randomRoundRobin
  , retrying'
  ) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Control.Monad.State
import           Control.Retry

import           Data.Default (def)
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client

import           P

import           Snooze.Balance.Data
import           Snooze.Url

import           System.IO
import           System.Random.Shuffle

import           Twine.Snooze

import           X.Control.Monad.Trans.Either

-- | Re-export of http-client default request so users don't have to
-- import Data.Default, with a more informative name.
newRequest :: Request
newRequest = def

balanceRequest :: BalanceEntry -> Request
balanceRequest (BalanceEntry (Host h) (Port p)) =
  requestCreate h p

balanceRequest' :: BalanceEntry -> Request -> Request
balanceRequest' (BalanceEntry (Host h) (Port p)) r =
  r {
    host = T.encodeUtf8 h
  , port = p
  }

balance :: MonadIO m =>
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

randomRoundRobin' :: (Monad m, MonadRandom m, MonadIO m) =>
   RetryPolicy -> ([e] -> a -> m (Either e b)) -> [a] -> m (Maybe b, [e])
randomRoundRobin' _ _ [] =
  return (Nothing, [])
randomRoundRobin' policy f l =
  fmap (\(b, e) -> (b, drop 1 e)) . flip evalStateT [] . retrying' policy $ \e -> do
    let go = get >>= \case
          [] -> do
            lift (shuffleM l) >>= put
            -- Loop internally - we know the list can't be empty
            go
          h : t -> do
            put t
            lift $ f e h
    go

-- | Ignore the errors from "randomRoundRobin'"
randomRoundRobin :: (Monad m, MonadRandom m, MonadIO m) =>
   RetryPolicy -> ([e] -> a -> m (Either e b)) -> [a] -> m (Maybe b)
randomRoundRobin policy f =
  fmap fst . randomRoundRobin' policy f

-- FIX Should be lifted in to x-retry (or thereabouts)
retrying' :: (MonadIO m) => RetryPolicy -> ([e] -> m (Either e b)) -> m (Maybe b, [e])
retrying' policy f =
  flip execStateT (Nothing, []) . retrying policy (\_ e -> return . isLeft $ e) $ \_ -> do
    (_, e) <- get
    r <- lift $ f e
    put $ case r of
      Left l -> (Nothing, l : e)
      Right b -> (Just b, e)
    return r
