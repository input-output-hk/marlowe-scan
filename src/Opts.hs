{-# LANGUAGE DeriveGeneric #-}

module Opts
  ( TitleLabel (..)
  , BlockExplorerHost (..)
  , ExplorerPort (..)
  , RuntimeHost (..)
  , RuntimePort (..)
  , Options (..)
  , mkUrlPrefix
  , parseOpts
  )
  where

import Control.Newtype.Generics (Newtype, op)
import GHC.Generics (Generic)
import Options.Applicative ( (<**>), auto, header, help, info, long, metavar,
                             option, showDefault, strOption, value, execParser,
                             helper, Parser )
import Text.Printf (printf)

newtype TitleLabel = TitleLabel String
  deriving (Generic, Show)

instance Newtype TitleLabel

newtype ExplorerPort = ExplorerPort Int
  deriving (Generic, Show)

instance Newtype ExplorerPort

newtype RuntimeHost = RuntimeHost String
  deriving (Generic, Show)

instance Newtype RuntimeHost

newtype RuntimePort = RuntimePort Int
  deriving (Generic, Show)

instance Newtype RuntimePort

newtype BlockExplorerHost = BlockExplorerHost String
  deriving (Generic, Show)

instance Newtype BlockExplorerHost

data Options = Options
  { optTitleLabel :: TitleLabel
  , optExplorerPort :: ExplorerPort
  , optRuntimeHost :: RuntimeHost
  , optRuntimePort :: RuntimePort
  , optBlockExplorerHost :: BlockExplorerHost
  }
  deriving Show

parser :: Parser Options
parser = Options
  <$> ( TitleLabel <$> strOption
        (  long "title-label"
        <> metavar "TEXT"
        <> help ("Label to be shown together with the title in the Marlowe Explorer in parenthesis. " ++
                 "(It can be used to display the name of the network that the Marlowe explorer is deployed to.)")
        <> showDefault
        <> value "Preprod"
        )
      )
  <*> ( ExplorerPort <$> option auto
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
  <*> ( BlockExplorerHost <$> strOption
        (  long "block-explorer"
        <> metavar "HOST"
        <> help "Host for exploring Cardano blockchain addresses, transactions, etc."
        <> showDefault
        <> value "preprod.cardanoscan.io"
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
