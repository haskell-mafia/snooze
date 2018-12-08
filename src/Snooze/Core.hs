{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_http_client(0,5,0)
#else
      checkStatus = _checkStatusIgnore,
#endif
    -- Never follow redirects - should always be done by the consumer explicitly if appropriate
      redirectCount = 0
    } mgr
  where
    -- A stupid default of http-client is to throw exceptions for non-200
    _checkStatusIgnore _ _ _ = Nothing

httpGo' :: Request -> IO (Response BSL.ByteString)
httpGo' req =
  newManager defaultManagerSettings >>= flip httpGo req
