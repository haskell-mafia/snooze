{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Http (
    get
  ) where


import           Network.HTTP.Client (Request (..))

import           P

data SnoozeHttpError =
  SnoozeHttpError

get :: BalanceConfig -> SnoozeURI -> EitherT SnoozeHttpError IO
get =
  P.error "todo"

  {--
  res <- bimapEitherT BorisHttpClientBalanceError id . newEitherT . runBalanceT b . httpBalanced $ \r -> r {
      H.path = encodePathSegmentsBS url
    , H.requestHeaders = [(HTTP.hAccept, borisVersion)]
    , H.method = HTTP.methodGet
    }
--}

request :: SnoozeURI -> SnoozeURIEitherT SnoozeHttpError IO Request
request =
  P.error "todo"
