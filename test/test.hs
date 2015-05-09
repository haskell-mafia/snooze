{-# LANGUAGE NoImplicitPrelude #-}

import           Disorder.Core.Main

import qualified Test.Snooze.Url

import           System.IO (IO)

main :: IO ()
main = disorderMain [
      Test.Snooze.Url.tests
    ]
