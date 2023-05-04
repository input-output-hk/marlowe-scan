{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Marlowe.Runtime.Types.TransactionsJSON (Transaction(..), Links(..), Resource(..), Block(..), Transactions(..), getContractTransactionsByContractId, getContractTransactionsByLink) where

import Data.Map (Map)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value, withObject, (.:), (.:?))
import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestHeader, httpLBS, getResponseBody, getResponseHeader)
import Data.Aeson (eitherDecode)
import Network.HTTP.Types (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)
import Language.Marlowe.Runtime.Types.General (Range(..), setRangeHeader, parseRangeHeader)
import Network.HTTP.Client (HttpException)
import Control.Error ( hoistEither, runExceptT, ExceptT(..) )
import Data.List (foldl')
import Control.Exception (try, Exception (displayException))
import Data.Either.Extra (mapLeft)

data Transaction = Transaction
  { links :: Links
  , resource :: Resource
  } deriving (Show, Eq)

newtype Links = Links
  { transaction :: String
  } deriving (Show, Eq)

data Resource = Resource
  { block :: Block
  , contractId :: String
  , status :: String
  , tags :: Map String Value
  , transactionId :: String
  , utxo :: Maybe String
  } deriving (Show, Eq)

data Block = Block
  { blockHeaderHash :: String
  , blockNo :: Integer
  , slotNo :: Integer
  } deriving (Show, Eq)

newtype Transactions = Transactions
  { transactions :: [Transaction]
  } deriving (Show, Eq)

instance FromJSON Transaction where
  parseJSON :: Value -> Parser Transaction
  parseJSON = withObject "Transaction" $ \o -> do
    links' <- o .: "links"
    resource' <- o .: "resource"
    return Transaction { links = links'
                       , resource = resource' }

instance FromJSON Links where
  parseJSON :: Value -> Parser Links
  parseJSON = withObject "Links" $ \o -> do
    transaction' <- o .: "transaction"
    return Links { transaction = transaction' }

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \o -> do
    block' <- o .: "block"
    contractId' <- o .: "contractId"
    status' <- o .: "status"
    tags' <- o .: "tags"
    transactionId' <- o .: "transactionId"
    utxo' <- o .:? "utxo"
    return Resource { block = block'
                    , contractId = contractId'
                    , status = status'
                    , tags = tags'
                    , transactionId = transactionId'
                    , utxo = utxo' }

instance FromJSON Block where
  parseJSON :: Value -> Parser Block
  parseJSON = withObject "Block" $ \o -> do
    blockHeaderHash' <- o .: "blockHeaderHash"
    blockNo' <- o .: "blockNo"
    slotNo' <- o .: "slotNo"
    return Block { blockHeaderHash = blockHeaderHash'
                 , blockNo = blockNo'
                 , slotNo = slotNo' }

instance FromJSON Transactions where
  parseJSON :: Value -> Parser Transactions
  parseJSON = withObject "Transactions" $ \o -> do
    results <- o .: "results"
    return Transactions { transactions = results }

getContractTransactionsByLink :: String -> String -> IO (Either String Transactions)
getContractTransactionsByLink endpoint link = do
  ts <- runExceptT $ getContractTransactions endpoint link Start
  return $ (\x -> Transactions { transactions = x }) <$> ts

getContractTransactions :: String -> String -> Range -> ExceptT String IO [Transaction]
getContractTransactions _endpoint _link Done = return []
getContractTransactions endpoint link range = do
  initialRequest <- parseRequest $ endpoint <> link
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  response <- ExceptT $ httpExceptionToString <$> try (httpLBS request)
  (Transactions { transactions = ts }) <- hoistEither $ eitherDecode (getResponseBody response)
  let nextRange = parseRangeHeader $ getResponseHeader "Next-Range" response
  (ts ++) <$> getContractTransactions endpoint link nextRange
  where
    httpExceptionToString :: Either HttpException a -> Either String a
    httpExceptionToString = mapLeft displayException

getContractTransactionsByContractId :: String -> String -> IO (Either String Transactions)
getContractTransactionsByContractId endpoint reqContractId =
    getContractTransactionsByLink endpoint $ "contracts/" ++ unpack (decodeUtf8 reqContractIdUrlEncoded) ++ "/transactions"
    where
      reqContractIdUrlEncoded = urlEncode False $ encodeUtf8 $ pack reqContractId
