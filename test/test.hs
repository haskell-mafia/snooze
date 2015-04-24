{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Orphanarium.Core.Main

import qualified Test.Snooze.Url

import           System.IO (IO)

main :: IO ()
main = orphanariumMain
    [ Test.Snooze.Url.tests
    ]
