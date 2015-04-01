{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Snooze.UrlTest where

import           Data.List as L
import           Data.Text as T

import           Orphanarium.Core

import           P

import           Snooze.Arbitrary ()
import           Snooze.Url

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_path_no_slash :: Path -> Property
prop_path_no_slash p =
  listToMaybe (pathToString p) =/= Just '/'

prop_path_slashes :: [Text] -> Property
prop_path_slashes p =
  L.isInfixOf "//" (pathToString (path p)) === False

prop_path_raw :: Path -> Property
prop_path_raw p =
  pathRaw (T.pack . pathToString $ p) === p


return []
tests :: IO Bool
tests = $quickCheckAll
