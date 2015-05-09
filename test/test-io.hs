{-# LANGUAGE NoImplicitPrelude #-}

import           Disorder.Core.Main

import qualified Test.Snooze.Core
import qualified Test.Snooze.Json
import qualified Test.Snooze.Text

import           System.IO (IO)

main :: IO ()
main = disorderMain [
      Test.Snooze.Core.tests
    , Test.Snooze.Json.tests
    , Test.Snooze.Text.tests
    ]
