{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.TextTest where

import           Data.Text as T
import qualified Data.Text.Lazy as TL

import           P

import           Snooze.Arbitrary ()
import           Snooze.Core as C
import           Snooze.CoreTest
import           Snooze.Text
import           Snooze.Server
import           Snooze.Url

import           System.IO

import           Web.Scotty as S

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


-- This is so bullshit I will copy and paste the coment from another file:
--   This is bullshit - you can't create a Response directly :(
prop_textResponse :: Text -> Path -> Property
prop_textResponse t p = monadicIO $ do
  r <- run . withServer' p
    (S.get (pathRoutePattern p) . S.text . TL.fromStrict $ t) $ \u ->
       C.get u []

  stop $ textBody r === t


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})