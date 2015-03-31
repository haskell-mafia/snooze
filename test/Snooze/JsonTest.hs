{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.JsonTest where

import           Control.Lens

import           Data.Map
import           Data.String (String)
import           Data.Text

import           Network.HTTP.Types.Status

import           P

import           Snooze.Arbitrary ()
import           Snooze.Json as J
import           Snooze.Server
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


prop_get :: [String] -> Path -> Property
prop_get j p = monadicIO $ do
  x <- run . withServer
    (S.get (pathRoutePattern p) $ S.json j) $ \b ->
      J.get $ url b p

  stop $ x === Right (Right j)

prop_get_status :: Path -> Property
prop_get_status p = monadicIO $ do
  x <- run . withServer
    (S.get (pathRoutePattern p) $ S.status status400) $ \b ->
      J.get (url b p)

  stop $ x === (Left status400 :: Either Status (Either Text ()))

prop_get_decode_failure :: [String] -> Path -> Property
prop_get_decode_failure j p = monadicIO $ do
  (x :: Either Status (Either Text (Map String Int))) <- run . withServer
    (S.get (pathRoutePattern p) $ S.json j) $ \b ->
      J.get (url b p)

  stop $ (over _Right isLeft) x === Right True

prop_post :: [String] -> Path -> Property
prop_post j p = monadicIO $ do
  x <- run . withServer
    (S.post (pathRoutePattern p) $ jsonData >>= (\(l :: [String]) -> S.json l)) $ \b ->
      J.post (url b p) j

  stop $ x === Right (Right j)

prop_post_status :: [String] -> Path -> Property
prop_post_status j p = monadicIO $ do
  x <- run . withServer
    (S.post (pathRoutePattern p) $ S.status status400) $ \b ->
      J.post (url b p) j

  stop $ x === (Left status400 :: Either Status (Either Text ()))

prop_post_decode_failure :: [String] -> Map String String -> Path -> Property
prop_post_decode_failure j m p = monadicIO $ do
  (x :: Either Status (Either Text [String])) <- run . withServer
    (S.post (pathRoutePattern p) $ jsonData >>= (\(_ :: [String]) -> S.json m)) $ \b ->
      J.post (url b p) j

  stop $ (over _Right isLeft) x === Right True


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
