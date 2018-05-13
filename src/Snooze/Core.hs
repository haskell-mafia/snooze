{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Core (
    httpGo
  , httpGo'
  ) where

import           Data.ByteString.Lazy as BSL

import           Network.HTTP.Client

import           P

import           System.IO


-- Eventually we will want/need to have sensible retries/timeouts here
httpGo :: Manager -> Request -> IO (Response BSL.ByteString)
httpGo mgr req =
  httpLbs req {
    -- Never follow redirects - should always be done by the consumer explicitly if appropriate
      redirectCount = 0
    } mgr
httpGo' :: Request -> IO (Response BSL.ByteString)
httpGo' req =
  newManager defaultManagerSettings >>= flip httpGo req
