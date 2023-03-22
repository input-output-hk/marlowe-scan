{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Types.ContractJSON
  ( Block(..)
  , ContractJSON(..)
  , Links(..)
  , Resource(..)
  , getContractJSON
  ) where

import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode, Value, (.:?), Object )
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)

import Language.Marlowe.Semantics.Types ( Contract(..), State(..) )
import Data.Map (Map)
import Data.Aeson.Types (Parser)


data ContractJSON = ContractJSON
  { links :: Links
  , resource :: Resource
  } deriving (Show, Eq)

newtype Links = Links
  { transactions :: String
  } deriving (Show, Eq)

data Resource = Resource
  { block :: Block
  , contractId :: String
  , currentContract :: Maybe Contract
  , initialContract :: Contract
  , metadata :: Object
  , roleTokenMintingPolicyId :: String
  , state :: Maybe State
  , status :: String
  , tags :: Map String String
  , version :: String
  } deriving (Show, Eq)

data Block = Block
  { blockHeaderHash :: String
  , blockNo :: Integer
  , slotNo :: Integer
  } deriving (Show, Eq)

instance FromJSON ContractJSON where
  parseJSON :: Value -> Parser ContractJSON
  parseJSON = withObject "Result" $ \o -> do
    links' <- o .: "links"
    resource' <- o .: "resource"
    return ContractJSON { links = links'
                        , resource = resource' }

instance FromJSON Links where
  parseJSON :: Value -> Parser Links
  parseJSON = withObject "Links" $ \o -> do
    transactions' <- o .: "transactions"
    return Links { transactions = transactions' }

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \o -> do
    block' <- o .: "block"
    contractId' <- o .: "contractId"
    currentContract' <- o .: "currentContract"
    initialContract' <- o .: "initialContract"
    metadata' <- o .: "metadata"
    roleTokenMintingPolicyId' <- o .: "roleTokenMintingPolicyId"
    state' <- o .:? "state"
    status' <- o .: "status"
    tags' <- o .: "tags"
    version' <- o .: "version"
    return Resource { block = block'
                    , contractId = contractId'
                    , currentContract = currentContract'
                    , initialContract = initialContract'
                    , metadata = metadata'
                    , roleTokenMintingPolicyId = roleTokenMintingPolicyId'
                    , state = state'
                    , status = status'
                    , tags = tags'
                    , version = version' }

instance FromJSON Block where
  parseJSON :: Value -> Parser Block
  parseJSON = withObject "Block" $ \o -> do
    blockHeaderHash' <- o .: "blockHeaderHash"
    blockNo' <- o .: "blockNo"
    slotNo' <- o .: "slotNo"
    return Block { blockHeaderHash = blockHeaderHash'
                 , blockNo = blockNo'
                 , slotNo = slotNo'}

getContractJSON :: String -> String -> IO (Either String ContractJSON)
getContractJSON endpoint reqContractId = do
    let reqContractIdUrlEncoded = urlEncode False $ encodeUtf8 $ pack reqContractId
    initialRequest <- parseRequest $ endpoint ++ "contracts/" ++ unpack (decodeUtf8 reqContractIdUrlEncoded)
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)

