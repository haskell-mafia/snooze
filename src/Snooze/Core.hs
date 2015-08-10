{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Core (
    get
  , post
  , delete
  , httpGo
  , httpGo'
  ) where

import           Data.ByteString.Lazy as BSL

import           Network.HTTP.Client
import           Network.HTTP.Types

import           P

import           Snooze.Url

import           System.IO


get :: Url -> RequestHeaders -> IO (Response BSL.ByteString)
get url' headers =
  httpGo $ (urlToRequest url') {
    requestHeaders = headers
  , method = methodGet
  }

post :: Url -> RequestHeaders -> BSL.ByteString -> IO (Response BSL.ByteString)
post url' headers body' =
  httpGo $ (urlToRequest url') {
    requestHeaders = headers
  , requestBody = RequestBodyLBS body'
  , method = methodPost
  }

delete :: Url -> RequestHeaders -> IO (Response BSL.ByteString)
delete url' headers =
  httpGo $ (urlToRequest url') {
    requestHeaders = headers
  , method = methodDelete
  }

-- Eventually we will want/need to have sensible retries/timeouts here
httpGo' :: ManagerSettings -> Request -> IO (Response BSL.ByteString)
httpGo' ms req =
  newManager ms >>=
    httpLbs req { checkStatus = checkStatusIgnore }
    where
      -- A stupid default of http-client is to throw exceptions for non-200
      checkStatusIgnore _ _ _ = Nothing

httpGo :: Request -> IO (Response BSL.ByteString)
httpGo = httpGo' defaultManagerSettings
