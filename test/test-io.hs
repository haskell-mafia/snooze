{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Test.Snooze.Core
import qualified Test.Snooze.Json
import qualified Test.Snooze.Text

import           Orphanarium.Core.Main

import           System.IO (IO)

main :: IO ()
main = orphanariumMain
    [ Test.Snooze.Core.tests
    , Test.Snooze.Json.tests
    , Test.Snooze.Text.tests
    ]
