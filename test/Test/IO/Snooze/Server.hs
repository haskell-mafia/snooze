{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Snooze.Server where

import           Control.Concurrent
import           Control.Exception
import           Control.Retry

import           Network
import           Network.HTTP.Client (Request)
import           Network.Wai (rawPathInfo)

import           P

import           Snooze.Url

import           System.IO
import           System.Random (randomRIO)

import           Web.Scotty hiding (request)


withServer :: ScottyM () -> (Request -> IO a) -> IO a
withServer app f = do
  -- FIX Find an open port rather than failing randomly
  port <- randomRIO (10100, 65534)
  -- Check that we can connect first to avoid flakey "connection refused"
  let connect' = bracket (connectTo "localhost" $ PortNumber (fromInteger $ toInteger port)) hClose pure
  bracket
    (forkIO . scotty port $ app)
    killThread
    (const $ (recoverAll (limitRetries 5) (\_ -> connect')) >> f (requestCreate "localhost" port))

-- Unfortunately the "Web.Scotty.Route" 'literal' match doesn't always match
pathRoutePattern :: Path -> RoutePattern
pathRoutePattern p =
  function $ \r ->
    if (show $ "/" <> pathToString p) == (show $ rawPathInfo r) then (Just []) else Nothing
