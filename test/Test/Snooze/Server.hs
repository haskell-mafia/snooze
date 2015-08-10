{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Snooze.Server where

import           Control.Concurrent
import           Control.Exception
import           Control.Retry

import           Data.Text as T

import           Network
import           Network.Wai (rawPathInfo)

import           P

import           Snooze.Url

import           System.IO
import           System.Posix.Process

import           Web.Scotty


withServer :: ScottyM () -> (Text -> IO a) -> IO a
withServer app f = do
  -- Check that we can connect first to avoid flakey "connection refused"
  bracket
    (forkIO . scotty 0 $ app)
    killThread
    (const $ (recoverAll (limitRetries 5) connect') >>= (\p -> f (url' p)))
 where
  connect' = do
    port <- getProcessID >>= findPort
    bracket (connectTo "localhost" port) hClose pure

  findPort _pid = hPutStrLn stderr "Implement me!" >> pure (PortNumber 80)

  url' port = T.concat ["http://localhost:", T.pack (show port), "/"]

withServer' :: Path -> ScottyM () -> (Url -> IO a) -> IO a
withServer' p s a =
  withServer s $ \b -> maybe (fail $ "Bad URL" <> T.unpack b) pure (url b p) >>= a

-- Unfortunately the "Web.Scotty.Route" 'literal' match doesn't always match
pathRoutePattern :: Path -> RoutePattern
pathRoutePattern p =
  function $ \r ->
    if (show $ "/" <> pathToString p) == (show $ rawPathInfo r) then (Just []) else Nothing
