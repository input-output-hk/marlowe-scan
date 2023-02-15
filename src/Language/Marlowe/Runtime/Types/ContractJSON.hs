{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.ContractJSON
  ( Block(..)
  , ContractJSON(..)
  , Links(..)
  , Resource(..)
  , Transaction(..)
  , Transactions(..)
  , getContractJSON
  , getContractTransactions
  ) where

import Language.Marlowe.Semantics.Types ( Contract(..), State(..) )
import Data.Aeson ( withObject, (.:?), (.:), FromJSON(parseJSON), eitherDecode )
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)

data ContractJSON = ContractJSON {
    links :: Links,
    resource :: Resource
} deriving (Show, Eq)

newtype Links = Links {
    transactions :: String
} deriving (Show, Eq)

data Resource = Resource {
    block :: Block,
    contractId :: String,
    currentContract :: Maybe Contract,
    initialContract :: Contract,
    roleTokenMintingPolicyId :: String,
    state :: Maybe State,
    status :: String,
    version :: String
} deriving (Show, Eq)

data Block = Block {
    blockHeaderHash :: String,
    blockNo :: Integer,
    slotNo :: Integer
} deriving (Show, Eq)

instance FromJSON ContractJSON where
    parseJSON = withObject "JSON" $ \v -> ContractJSON
        <$> v .: "links"
        <*> v .: "resource"

instance FromJSON Links where
    parseJSON = withObject "Links" $ \v -> Links
        <$> v .: "transactions"

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \v -> Resource
        <$> v .: "block"
        <*> v .: "contractId"
        <*> v .:? "currentContract"
        <*> v .: "initialContract"
        <*> v .: "roleTokenMintingPolicyId"
        <*> v .:? "state"
        <*> v .: "status"
        <*> v .: "version"

instance FromJSON Block where
    parseJSON = withObject "Block" $ \v -> Block
        <$> v .: "blockHeaderHash"
        <*> v .: "blockNo"
        <*> v .: "slotNo"

getContractJSON :: String -> String -> IO (Either String ContractJSON)
getContractJSON endpoint reqContractId = do
    let reqContractIdUrlEncoded = urlEncode False $ encodeUtf8 $ pack reqContractId
    initialRequest <- parseRequest $ endpoint ++ "contracts/" ++ unpack (decodeUtf8 reqContractIdUrlEncoded)
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)

data Transaction = Transaction
  { txLink :: String
  , txBlock :: Block
  , txContractId :: String
  , txTransactionId :: String
  } deriving (Show, Eq)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    link <- ((o .: "links") >>= (.: "transaction"))
    res <- o .: "resource"
    Transaction
      <$> pure link
      <*> res .: "block"
      <*> res .: "contractId"
      <*> res .: "transactionId"

newtype Transactions = Transactions [Transaction]
  deriving (Show, Eq)

instance FromJSON Transactions where
  parseJSON = withObject "Transactions" $ \o -> do
    Transactions <$> o .: "results"

getContractTransactions :: String -> String -> IO (Either String Transactions)
getContractTransactions endpoint link = do
    initialRequest <- parseRequest $ endpoint ++ link
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)
