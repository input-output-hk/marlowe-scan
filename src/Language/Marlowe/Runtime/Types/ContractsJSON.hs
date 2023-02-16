{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractList(..)
  , ContractInList(..)
  , Resource(..)
  , getContracts
  )
  where

import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode )
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestMethod)

import Language.Marlowe.Runtime.Types.Common ( Block, Link(..) )


newtype ContractList = ContractList [ContractInList]
  deriving (Eq, Show)

instance FromJSON ContractList where
  parseJSON = withObject "ContractList" $ \o -> do
    ContractList <$> o .: "results"

data ContractInList = ContractInList
  { cilLink :: Link
  , cilResource :: Resource
  }
  deriving (Eq, Show)

instance FromJSON ContractInList where
  parseJSON = withObject "ContractInList" $ \o -> ContractInList
    <$> (Link <$> (o .: "links" >>= (.: "contract")))
    <*> o .: "resource"

data Resource = Resource
  { resContractId :: String
  , resBlock :: Block
  }
  deriving (Eq, Show)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o -> Resource
    <$> o .: "contractId"
    <*> o .: "block"

getContracts :: String -> IO (Either String ContractList)
getContracts endpoint = do
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
  response <- httpLBS request
  return $ eitherDecode (getResponseBody response)
