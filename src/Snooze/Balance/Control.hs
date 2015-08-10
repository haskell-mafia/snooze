{-# LANGUAGE NoImplicitPrelude #-}
module Snooze.Balance.Control (
    module X
  , BalanceConfig (..)
  , BalanceError (..)
  , BalanceT
  , runBalanceT
  ) where

import           Control.Monad.Trans.Either as X
import           Control.Monad.Trans.Reader as X
import           Control.Retry

import           Network.HTTP.Client

import           P

import           Snooze.Balance.Data


data BalanceConfig =
  BalanceConfig {
    remoteBalanceTable :: UpdatableBalanceTable
  , remoteRetryPolicy :: RetryPolicy
  , balanceConfigHttpManager :: Manager
  }

data BalanceError =
    BalanceTimeout [HttpException]
  deriving (Show)

type BalanceT m = ReaderT BalanceConfig (EitherT BalanceError m)


runBalanceT :: r -> ReaderT r (EitherT e m) a -> m (Either e a)
runBalanceT c = runEitherT . flip runReaderT c
