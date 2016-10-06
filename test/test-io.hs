{-# LANGUAGE NoImplicitPrelude #-}

import           Disorder.Core.Main

import qualified Test.IO.Snooze.Balance.Http
import qualified Test.IO.Snooze.Core
import qualified Test.IO.Snooze.Json
import qualified Test.IO.Snooze.Text

import           System.IO (IO)

main :: IO ()
main = disorderMain [
      Test.IO.Snooze.Balance.Http.tests
    , Test.IO.Snooze.Core.tests
    , Test.IO.Snooze.Json.tests
    , Test.IO.Snooze.Text.tests
    ]
