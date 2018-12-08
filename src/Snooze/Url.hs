{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
module Snooze.Url (
    Path(pathToString)
  , requestCreate
  , path
  , pathRaw
  , encodePathSegmentsBS
  , encodePathBS
  , newRequest
  ) where

import           Blaze.ByteString.Builder (toLazyByteString)

import           Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String (String)
import           Data.Text as T
import           Data.Text.Encoding as T

import           Network.HTTP.Client (Request)
#if MIN_VERSION_http_client(0,5,0)
import           Network.HTTP.Client (defaultRequest)
#else
import           Data.Default
#endif
import           Network.HTTP.Client.Internal (host, port)
import           Network.HTTP.Types.URI as URI (encodePathSegments, encodePathSegmentsRelative)

import           P


-- | Represents _just_ the relative path of a URL.
--
-- Invariant: Will _never_ start with a leading slash.
newtype Path = Path {
    pathToString :: String
  } deriving (Eq, Show)


requestCreate :: Text -> Int -> Request
requestCreate host' port' = newRequest {
    host = T.encodeUtf8 host'
  , port = port'
  }

-- | Re-export of http-client default request so users don't have to
-- import Data.Default, with a more informative name.
newRequest :: Request
#if MIN_VERSION_http_client(0,5,0)
newRequest = defaultRequest
#else
newRequest = def
#endif

-- | Construct a safe 'Path' from unescaped segments.
--
-- This supports a limited "safe" set of paths, which includes strips blank segments.
path :: [Text] -> Path
path =
  Path . BSL.unpack . toLazyByteString . encodePathSegmentsRelative . P.filter (not . T.null)

-- | Construct a 'Path' from an escaped 'Text'.
--
-- NOTE: This will not escape anything, see 'path'.
pathRaw :: Text -> Path
pathRaw =
  Path . T.unpack . T.dropWhile (== '/')

-- | For setting 'queryString' on http-client "Request"
encodePathSegmentsBS :: [Text] -> ByteString
encodePathSegmentsBS =
  BSL.toStrict . toLazyByteString . URI.encodePathSegments

encodePathBS :: Path -> ByteString
encodePathBS (Path p) =
  T.encodeUtf8 . T.pack $ "/" <> p
