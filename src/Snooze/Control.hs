{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Control (
    SnoozeIO
  , statusHandler
  ) where

import           Control.Exception (throwIO)

import           Network.HTTP.Client (HttpException(StatusCodeException))
import           Network.HTTP.Types.Status (Status)

import           P

import           System.IO


-- FIX Better monad stack - EitherT perhaps?
type SnoozeIO a = IO (Either Status a)


statusHandler :: HttpException -> SnoozeIO b
statusHandler (StatusCodeException s _ _) = pure $ Left s
statusHandler e = throwIO e
