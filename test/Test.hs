{-# LANGUAGE OverloadedStrings #-}

module Test where

import qualified QuickCheck as QC
import qualified HUnit as H

import Options.Applicative
import Control.Monad

main :: IO ()
main = join $ customExecParser (prefs $ disambiguate <> showHelpOnError <> showHelpOnEmpty) $ info (helper <*> liftA2 (<>) qcOptions hOptions) fullDesc
  where
    qcOptions = QC.runTests
      <$> option auto (long "quickcheck-tries" <> short 't' <> metavar "COUNT" <> value 10000 <> showDefault <> help "Number of tests for QuickCheck tests")
    hOptions = H.runTests
      <$> strOption (long "ip" <> short 'i' <> metavar "IP" <> value "localhost" <> showDefault <> help "The ip address of the server")
      <*> strOption (long "port" <> short 'p' <> metavar "PORT or HOSTNAME" <> value "25575" <> showDefault <> help "The RCon port of the server")
      <*> option auto (long "password" <> short 'P' <> metavar "PASSWORD" <> value "assembler" <> showDefault <> help "RCon password for the server")
      <*> strArgument (metavar "SERVER_FOLDER" <> help "Folder where the server jar is located")