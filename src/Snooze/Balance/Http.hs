{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snooze.Balance.Http (
    httpBalanced
  , logRequestResponse
  , asLogRequestResponse
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import           P

import           Snooze.Balance.Control
import           Snooze.Balance.Core
import           Snooze.Balance.Data
import           Snooze.Core

import           System.IO (IO, putStrLn)


httpBalanced ::
    (Applicative m, MonadIO m)
  => (Request -> Request)
  -> BalanceT m (Request, Response BSL.ByteString)
httpBalanced req = ReaderT $ \(BalanceConfig ubt rp mgr) -> EitherT $ do
  bt <- getTable ubt
  let httpWithReq req' = fmap ((,) req') $ httpGo mgr req'
  liftIO . fmap (\(m, e) -> maybeToRight (BalanceTimeout e) m) $ randomRoundRobin' rp
    (\_ b ->
      catch (fmap Right . httpWithReq . req . balanceRequest $ b)
      (\(e :: HttpException) -> pure $ Left e))
    (balanceTableList bt)

logRequestResponse :: Text -> Request -> Response a -> IO ()
logRequestResponse t req resp =
  putStrLn . T.unpack $ asLogRequestResponse t req resp

asLogRequestResponse :: Text -> Request -> Response a -> Text
asLogRequestResponse t req resp =
       t
    <> ": "
    <> T.decodeUtf8 (HC.host req)
    <> ":"
    <> (T.pack . show . HC.port) req
    <> " "
    <> T.decodeUtf8 (HC.method req)
    <> " "
    <> T.decodeUtf8 (HC.path req)
    <> " "
    <> (T.pack . show . HT.statusCode . HC.responseStatus) resp
    <> " "
    <> (T.decodeUtf8 . HT.statusMessage . HC.responseStatus) resp
