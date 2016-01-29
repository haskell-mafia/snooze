{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Core (
    httpGo
  , httpGo'
  , httpSafe
  ) where

import           Data.ByteString.Lazy as BSL

import           Network.HTTP.Client

import           P

import           System.IO


-- Eventually we will want/need to have sensible retries/timeouts here
httpGo :: Manager -> Request -> IO (Response BSL.ByteString)
httpGo mgr =
  flip httpLbs mgr . httpSafe

httpGo' :: Request -> IO (Response BSL.ByteString)
httpGo' req =
  newManager defaultManagerSettings >>= flip httpGo req

httpSafe :: Request -> Request
httpSafe req =
  req {
      checkStatus = checkStatusIgnore
    -- Never follow redirects - should always be done by the consumer explicitly if appropriate
    , redirectCount = 0
    }
  where
    -- A stupid default of http-client is to throw exceptions for non-200
    checkStatusIgnore _ _ _ = Nothing
