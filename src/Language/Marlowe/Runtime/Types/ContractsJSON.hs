{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractList(..), ContractInList(..)
  , Links(..)
  , Resource(..)
  , getContracts
  )
  where

import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode )
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestMethod)

import Language.Marlowe.Runtime.Types.Common  -- FIXME explicit imports


newtype ContractList = ContractList [ContractInList]
  deriving (Eq, Show)

instance FromJSON ContractList where
  parseJSON = withObject "ContractList" $ \o -> do
    ContractList <$> o .: "results"

data ContractInList = ContractInList
  { cilLinks :: Links
  , cilResource :: Resource
  }
  deriving (Eq, Show)

instance FromJSON ContractInList where
  parseJSON = withObject "ContractInList" $ \o -> ContractInList
    <$> o .: "links"
    <*> o .: "resource"

newtype Links = Links { linkUrl :: String }
  deriving (Show, Eq)

instance FromJSON Links where
  parseJSON = withObject "Links" $ \o -> Links
    <$> o .: "contract"

data Resource = Resource
  { resContractId :: String
  , resBlock :: Block
  -- FIXME Add these soon, unsure of the use-cases yet
  -- , cilMetadata
  -- , cilRoleTokenPolicyId
  -- , cilStatus
  -- , cilVersion
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
