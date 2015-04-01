{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Json (
    decodeResponse
  , headerJson
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy as BSL
import           Data.Text as T

import           Network.HTTP.Client
import           Network.HTTP.Types.Header

import           P


decodeResponse :: FromJSON a => Response BSL.ByteString -> Either Text a
decodeResponse = either (Left . T.pack) Right . eitherDecode . responseBody

headerJson :: Header
headerJson = ("Accept", "application/json; charset=utf-8")
