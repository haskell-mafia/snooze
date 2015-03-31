{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.CoreTest where

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


prop_delete :: Path -> Property
prop_delete p = monadicIO $ do
  x <- run . withServer
    (S.delete (pathRoutePattern p) $ pure ()) $ \b ->
      C.delete $ url b p

  stop $ x === Right ()

prop_delete_status :: Path -> Property
prop_delete_status p = monadicIO $ do
  x <- run . withServer
    (S.delete (pathRoutePattern p) $ S.status status400) $ \b ->
      C.delete (url b p)

  stop $ x === Left status400


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
