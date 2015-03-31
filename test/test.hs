{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Orphanarium.Core.Main

import           Snooze.UrlTest

import           System.IO (IO)

main :: IO ()
main = orphanariumMain
    [ Snooze.UrlTest.tests
    ]
