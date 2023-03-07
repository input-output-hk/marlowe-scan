{-# LANGUAGE DeriveGeneric #-}

module Opts
  ( BlockExplorerPrefix (..)
  , ExplorerPort (..)
  , RuntimeHost (..)
  , RuntimePort (..)
  , Options (..)
  , mkUrlPrefix
  , parseOpts
  )
  where

import Control.Newtype.Generics
import GHC.Generics hiding (Prefix)
import Options.Applicative ( (<**>), auto, header, help, info, long, metavar,
                             option, showDefault, strOption, value, execParser,
                             helper, Parser )
import Text.Printf (printf)

newtype ExplorerPort = ExplorerPort Int
  deriving (Generic, Show)

instance Newtype ExplorerPort

newtype RuntimeHost = RuntimeHost String
  deriving (Generic, Show)

instance Newtype RuntimeHost

newtype RuntimePort = RuntimePort Int
  deriving (Generic, Show)

instance Newtype RuntimePort

newtype BlockExplorerPrefix = BlockExplorerPrefix String
  deriving (Generic, Show)

instance Newtype BlockExplorerPrefix

data Options = Options
  { optExplorerPort :: ExplorerPort
  , optRuntimeHost :: RuntimeHost
  , optRuntimePort :: RuntimePort
  , optBlockExplorerPrefix :: BlockExplorerPrefix
  }
  deriving Show

parser :: Parser Options
parser = Options
  <$> ( ExplorerPort <$> option auto
        (  long "explorer-port"
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
        <> metavar "PORT"
        <> help "Port number of the running Marlowe Runtime server"
        <> showDefault
        <> value 8080
        )
      )
  <*> ( BlockExplorerPrefix <$> strOption
        (  long "block-explorer-prefix"
        <> metavar "URL"
        <> help "Prefix URL for exploring Cardano blockchain addresses, transactions, etc."
        <> showDefault
        <> value "https://preprod.cardanoscan.io/transaction"
        )
      )

parseOpts :: IO Options
parseOpts = do
  execParser $ info (parser <**> helper)
    (  header "Marlowe Explorer server"
    )

mkUrlPrefix :: Options -> String
mkUrlPrefix opts =
  let
    rhost = op RuntimeHost . optRuntimeHost $ opts
    rport = op RuntimePort . optRuntimePort $ opts
  in
    printf "http://%s:%d/" rhost rport
