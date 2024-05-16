{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( Block(..)
  , ContractInList(..)
  , ContractListISeq
  , ContractLinks(..)
  , Resource(..)
  , ResultList(..)
  )
  where

import Control.Exception ( Exception(displayException) )
import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), Value )
import Network.HTTP.Simple ( HttpException )
import Language.Marlowe.Runtime.Types.IndexedSeq (IndexedSeq, Indexed (getIdentifier))
import Data.Aeson.Types (Parser)
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq

data Block = Block
  { blockHeaderHash :: String
  , blockNo :: Integer
  , slotNo :: Integer
  } deriving (Show, Eq)

instance FromJSON Block where
  parseJSON :: Value -> Parser Block
  parseJSON = withObject "Block" $ \o -> do
    blockHeaderHash' <- o .: "blockHeaderHash"
    blockNo' <- o .: "blockNo"
    slotNo' <- o .: "slotNo"
    return Block { blockHeaderHash = blockHeaderHash'
                 , blockNo = blockNo'
                 , slotNo = slotNo'}

data Resource = Resource
  { block :: Block
  , contractId :: String
  , roleTokenMintingPolicyId :: String
  , status :: String
  , version :: String
  } deriving (Show, Eq)

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \o ->
    Resource <$> o .: "block"
             <*> o .: "contractId"
             <*> o .: "roleTokenMintingPolicyId"
             <*> o .: "status"
             <*> o .: "version"

data ContractLinks = ContractLinks
  { contract :: String
  , transactions :: String
  } deriving (Show, Eq)

instance FromJSON ContractLinks where
  parseJSON :: Value -> Parser ContractLinks
  parseJSON = withObject "ContractLinks" $ \o ->
    ContractLinks <$> o .: "contract"
                  <*> o .: "transactions"

data ContractInList = ContractInList
  { links :: ContractLinks
  , resource :: Resource
  } deriving (Show, Eq)

instance ISeq.Indexed ContractInList String where
  getIdentifier :: ContractInList -> String
  getIdentifier = contractId . resource

instance FromJSON ContractInList where
  parseJSON :: Value -> Parser ContractInList
  parseJSON = withObject "ContractInList" $ \o ->
    ContractInList <$> o .: "links"
                   <*> o .: "resource"

newtype ResultList = ResultList
  { results :: [ContractInList]
  } deriving (Show, Eq)

instance FromJSON ResultList where
  parseJSON :: Value -> Parser ResultList
  parseJSON = withObject "ResultList" $ \o ->
    ResultList <$> o .: "results"

type ContractListISeq = IndexedSeq ContractInList String

data ContractListFetchingException = DecodingException String
                                   | RequestException HttpException
  deriving (Show)

instance Exception ContractListFetchingException where
  displayException :: ContractListFetchingException -> String
  displayException (DecodingException msg) = "Decoding exception: " ++ msg
  displayException (RequestException subException) = "Exception querying runtime for contracts: " ++ displayException subException

