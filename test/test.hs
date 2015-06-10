{-# LANGUAGE NoImplicitPrelude #-}

import           Disorder.Core.Main

import qualified Test.Snooze.Balance.Core
import qualified Test.Snooze.Balance.Serial
import qualified Test.Snooze.Url

import           System.IO (IO)

main :: IO ()
main = disorderMain [
      Test.Snooze.Balance.Core.tests
    , Test.Snooze.Balance.Serial.tests
    , Test.Snooze.Url.tests
    ]
