{-# LANGUAGE NoImplicitPrelude #-}
module Snooze.Balance.Core (
    balanceRequest
  ) where

import           Data.Text.Encoding as TL

import           Network.HTTP.Client

import           Snooze.Balance.Data


balanceRequest :: BalanceEntry -> Request -> Request
balanceRequest (BalanceEntry h p) r = r {
    host = TL.encodeUtf8 h
  , port = p
  }
