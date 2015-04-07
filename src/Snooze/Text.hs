{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Text (
    textResponse
  , textBody
  ) where

import           Data.ByteString.Lazy as BSL
import           Data.Text as T
import           Data.Text.Encoding as T

import           Network.HTTP.Client

import           P


textResponse :: Response BSL.ByteString -> Response Text
textResponse =
  fmap (T.decodeUtf8. toStrict)

textBody :: Response BSL.ByteString -> Text
textBody =
  responseBody . textResponse
