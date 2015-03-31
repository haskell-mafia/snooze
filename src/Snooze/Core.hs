{-# LANGUAGE NoImplicitPrelude #-}
module Snooze.Core (
    Snooze.Core.delete
  ) where

import           Control.Exception as E

import           Network.Wreq as R

import           P

import           Snooze.Control
import           Snooze.Url


delete :: Url -> SnoozeIO ()
delete url' =
  (fmap Right . void . R.delete $ urlToString url')
    `E.catch` statusHandler
