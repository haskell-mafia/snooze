{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_snooze

import           P

import           Snooze.Data

import           System.IO
import           System.Exit

import           X.Options.Applicative

data Command =
  GetCommand SnoozeUri
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser (SafeCommand Command)
parser =
  safeCommand . subparser . mconcat $ [
      command' "get" "Get specified uri." $
        GetCommand
          <$> snoozeUriP
    ]

run :: Command -> IO ()
run c = case c of
  GetCommand _ ->
    putStrLn "*implement me*" >> exitFailure

snoozeUriP :: Parser SnoozeUri
snoozeUriP =
  fmap SnoozeUri . argument textRead . mconcat $ [
      metavar "URI"
    , help "Request URI, e.g. snooze://service/path?query=1"
    ]
