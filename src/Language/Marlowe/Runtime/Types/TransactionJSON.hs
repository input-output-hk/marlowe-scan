{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Types.TransactionJSON (Link(..), Block(..), RoleToken(..), Token(..), Resource(..), Transaction(..), getTransaction) where

import Language.Marlowe.Semantics.Types (Contract, State, Input)
import Data.Map ( Map )
import Data.Aeson ( FromJSON(parseJSON), withObject, Value, withObject, Value, (.:?), eitherDecode )
import Data.Aeson.Types ((.:), Parser)
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Simple (setRequestMethod, setRequestHeader, httpLBS, getResponseBody)
import Data.Time (UTCTime)

data Link = Link
  { next :: Maybe String,
    previous :: Maybe String
  }
  deriving (Show)

data Block = Block
  { blockHeaderHash :: String,
    blockNo :: Integer,
    slotNo :: Integer
  }
  deriving (Show)

newtype RoleToken = RoleToken
  { role_token :: String
  }
  deriving (Show)

data Token = Token
  { currency_symbol :: String,
    token_name :: String
  }
  deriving (Show)

data Resource = Resource
  { block :: Block,
    consumingTx :: Maybe String,
    continuations :: Maybe String,
    contractId :: String,
    inputUtxo :: String,
    inputs :: [Input],
    invalidBefore :: UTCTime,
    invalidHereafter :: UTCTime,
    outputContract :: Maybe Contract,
    outputState :: Maybe State,
    outputUtxo :: Maybe String,
    status :: String,
    tags :: Map String Value,
    transactionId :: String
  }
  deriving (Show)

data Transaction = Transaction
  { links :: Link,
    resource :: Resource
  }
  deriving (Show)

instance FromJSON Link where
  parseJSON :: Value -> Parser Link
  parseJSON = withObject "Link" $ \v -> do
    next' <- v .:? "next"
    previous' <- v .:? "previous"
    return Link { next = next'
                , previous = previous' }

instance FromJSON Block where
  parseJSON :: Value -> Parser Block
  parseJSON = withObject "Block" $ \v -> do
    blockHeaderHash' <- v .: "blockHeaderHash"
    blockNo' <- v .: "blockNo"
    slotNo' <- v .: "slotNo"
    return Block { blockHeaderHash = blockHeaderHash'
                 , blockNo = blockNo'
                 , slotNo = slotNo' }

instance FromJSON RoleToken where
  parseJSON :: Value -> Parser RoleToken
  parseJSON = withObject "RoleToken" $ \v -> do
    role_token' <- v .: "role_token"
    return RoleToken { role_token = role_token' }

instance FromJSON Token where
  parseJSON :: Value -> Parser Token
  parseJSON = withObject "Token" $ \v -> do
    currency_symbol' <- v .: "currency_symbol"
    token_name' <- v .: "token_name"
    return Token { currency_symbol = currency_symbol'
                 , token_name = token_name' }

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \v -> do
    block' <- v .: "block"
    consumingTx' <- v .:? "consumingTx"
    continuations' <- v .:? "continuations"
    contractId' <- v .: "contractId"
    inputUtxo' <- v .: "inputUtxo"
    inputs' <- v .: "inputs"
    invalidBefore' <- v .: "invalidBefore"
    invalidHereafter' <- v .: "invalidHereafter"
    outputContract' <- v .:? "outputContract"
    outputState' <- v .:? "outputState"
    outputUtxo' <- v .:? "outputUtxo"
    status' <- v .: "status"
    tags' <- v .: "tags"
    transactionId' <- v .: "transactionId"
    return Resource { block = block'
                    , consumingTx = consumingTx'
                    , continuations = continuations'
                    , contractId = contractId'
                    , inputUtxo = inputUtxo'
                    , inputs = inputs'
                    , invalidBefore = invalidBefore'
                    , invalidHereafter = invalidHereafter'
                    , outputContract = outputContract'
                    , outputState = outputState'
                    , outputUtxo = outputUtxo'
                    , status = status'
                    , tags = tags'
                    , transactionId = transactionId' }

instance FromJSON Transaction where
  parseJSON :: Value -> Parser Transaction
  parseJSON = withObject "Transaction" $ \v -> do
    links' <- v .: "links"
    resource' <- v .: "resource"
    return Transaction { links = links'
                       , resource = resource' }

getTransaction :: String -> String -> String -> IO (Either String Transaction)
getTransaction endpoint tsLink tLink = do
    initialRequest <- parseRequest $ endpoint ++ tsLink ++ "/" ++ tLink
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)
