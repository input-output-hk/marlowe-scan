{-# LANGUAGE DeriveGeneric #-}

module Opts
  ( ExplorerPort (..)
  , RuntimePort (..)
  , Options (..)
  , parseOpts
  )
  where

import Control.Newtype.Generics
import GHC.Generics hiding (Prefix)
import Options.Applicative

newtype RuntimePort = RuntimePort Int
  deriving (Generic, Show)

instance Newtype RuntimePort

newtype ExplorerPort = ExplorerPort Int
  deriving (Generic, Show)

instance Newtype ExplorerPort

data Options = Options
  { optExplorerPort :: ExplorerPort
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
