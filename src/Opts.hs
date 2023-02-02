{-# LANGUAGE DeriveGeneric #-}

module Opts
  ( ExplorerPort (..)
  , RuntimeHost (..)
  , RuntimePort (..)
  , Options (..)
  , parseOpts
  )
  where

import Control.Newtype.Generics
import GHC.Generics hiding (Prefix)
import Options.Applicative

newtype ExplorerPort = ExplorerPort Int
  deriving (Generic, Show)

instance Newtype ExplorerPort

newtype RuntimeHost = RuntimeHost String
  deriving (Generic, Show)

instance Newtype RuntimeHost

newtype RuntimePort = RuntimePort Int
  deriving (Generic, Show)

instance Newtype RuntimePort

data Options = Options
  { optExplorerPort :: ExplorerPort
  , optRuntimeHost :: RuntimeHost
  , optRuntimePort :: RuntimePort
  }
  deriving Show

parser :: Parser Options
parser = Options
  <$> ( ExplorerPort <$> option auto
        (  long "explorer-port"
        <> short 'e'
        <> metavar "PORT"
        <> help "Port number to use for this Marlowe Explorer server"
        <> showDefault
        <> value 8081
        )
      )
  <*> ( RuntimeHost <$> strOption
        (  long "runtime-host"
        <> metavar "HOSTNAME-OR-IP"
        <> help "Hostname or IP of the running Marlowe Runtime server"
        <> showDefault
        <> value "builder"
        )
      )
  <*> ( RuntimePort <$> option auto
        (  long "runtime-port"
        <> short 'r'
        <> metavar "PORT"
        <> help "Port number of the running Marlowe Runtime server"
        <> showDefault
        <> value 8080
        )
      )

parseOpts :: IO Options
parseOpts = do
  execParser $ info (parser <**> helper)
    (  header "Marlowe Explorer server"
    )
