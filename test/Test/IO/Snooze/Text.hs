{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Snooze.Text where

import           Data.Text as T
import qualified Data.Text.Lazy as TL

import           P

import           Snooze.Core as C
import           Snooze.Text

import           System.IO

import           Web.Scotty as S

import           Test.Snooze.Arbitrary ()
import           Test.IO.Snooze.Server
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic


-- This is so bullshit I will copy and paste the coment from another file:
--   This is bullshit - you can't create a Response directly :(
prop_textResponse :: Text -> Property
prop_textResponse t = monadicIO $ do
  r <- run $ withServer (S.get "/" . S.text . TL.fromStrict $ t) httpGo'

  stop $ textBody r === t


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
