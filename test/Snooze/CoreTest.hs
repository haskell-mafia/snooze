{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.CoreTest where

import           Data.ByteString.Lazy as BSL
import           Data.Text as T

import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import           P

import           Snooze.Arbitrary ()
import           Snooze.Core as C
import           Snooze.Server
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


prop_get :: BSL.ByteString -> Path -> Property
prop_get bs p = monadicIO $ do
  x <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.raw bs) $ \u ->
       C.get u []

  stop $ responseBody x === bs

prop_get_status :: Path -> Property
prop_get_status p = monadicIO $ do
  x <- run . withServer' p
    (S.get (pathRoutePattern p) $ S.status status400) $ \u ->
      C.get u []

  stop $ responseStatus x === status400

prop_post :: BSL.ByteString -> Path -> Property
prop_post bs p = monadicIO $ do
  x <- run . withServer' p
    (S.post (pathRoutePattern p) $ S.body >>= S.raw) $ \u ->
      C.post u [] bs

  stop $ responseBody x === bs

prop_post_status :: BSL.ByteString -> Path -> Property
prop_post_status bs p = monadicIO $ do
  x <- run . withServer' p
    (S.post (pathRoutePattern p) $ S.status status400) $ \u ->
      C.post u [] bs

  stop $ responseStatus x === status400

prop_delete :: BSL.ByteString -> Path -> Property
prop_delete bs p = monadicIO $ do
  x <- run . withServer' p
    (S.delete (pathRoutePattern p) $ S.raw bs) $ \u ->
      C.delete u []

  stop $ responseBody x === bs

prop_delete_status :: Path -> Property
prop_delete_status p = monadicIO $ do
  x <- run . withServer' p
    (S.delete (pathRoutePattern p) $ S.status status400) $ \u ->
      C.delete u []

  stop $ responseStatus x === status400


withServer' :: Path -> ScottyM () -> (Url -> IO a) -> IO a
withServer' p s a =
  withServer s $ \b -> maybe (fail $ "Bad URL" <> T.unpack b) pure (url b p) >>= a

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
