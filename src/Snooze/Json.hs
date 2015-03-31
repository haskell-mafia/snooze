{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Json (
    SnoozeIO
  , Snooze.Json.get
  , Snooze.Json.post
  ) where

import           Control.Exception as E
import           Control.Lens

import           Data.Aeson
import           Data.ByteString.Lazy as BSL
import           Data.Text as T

import           Network.Wreq as R

import           P

import           Snooze.Control
import           Snooze.Url


get :: (FromJSON a) => Url -> SnoozeIO (Either Text a)
get url' =
  (decodeResponse =<< (R.getWith (defaults & headerJson) $ urlToString url'))
    `E.catch` statusHandler

post :: (ToJSON a, FromJSON b) => Url -> a -> SnoozeIO (Either Text b)
post url' a =
  (decodeResponse =<< (R.postWith (defaults & headerJson) (urlToString url') . toJSON $ a))
    `E.catch` statusHandler


decodeResponse :: FromJSON a => Response BSL.ByteString -> SnoozeIO (Either Text a)
decodeResponse r = pure . Right . over _Left T.pack . eitherDecode $ r ^. responseBody

headerJson :: Options -> Options
headerJson = header "Accept" .~ ["application/json; charset=utf-8"]
