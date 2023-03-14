{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractInList(..)
  , ContractList(..)
  , ContractLinks(..)
  , Resource(..)
  , refreshContracts
  )
  where

import Control.Exception ( try, Exception(displayException) )
import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode, Value )
import Data.ByteString ( ByteString )
import Data.List ( foldl' )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Network.HTTP.Simple ( HttpException, Request, parseRequest,
  getResponseBody, httpLBS, getResponseHeader, setRequestHeader,
  setRequestMethod )
import Language.Marlowe.Runtime.Types.IndexedSeq (IndexedSeq)
import Language.Marlowe.Runtime.Types.Common ( Block )
import Data.Aeson.Types (Parser)
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq
import Language.Marlowe.Runtime.Types.LazyFeed (LazyFeed)
import qualified Language.Marlowe.Runtime.Types.LazyFeed as LazyFeed

data Resource = Resource
  { block :: Block
  , continuations :: Maybe String
  , contractId :: String
  , roleTokenMintingPolicyId :: String
  , status :: String
  , version :: String
  } deriving (Show, Eq)

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \o ->
    Resource <$> o .: "block"
             <*> o .: "continuations"
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

instance FromJSON ContractInList where
  parseJSON :: Value -> Parser ContractInList
  parseJSON = withObject "ContractInList" $ \o ->
    ContractInList <$> o .: "links"
                   <*> o .: "resource"

instance ISeq.Indexed ContractInList String where
  getIdentifier :: ContractInList -> String
  getIdentifier = contractId . resource

newtype ResultList = ResultList
  { results :: [ContractInList]
  } deriving (Show, Eq)

instance FromJSON ResultList where
  parseJSON :: Value -> Parser ResultList
  parseJSON = withObject "ResultList" $ \o ->
    ResultList <$> o .: "results"

type ContractListISeq = IndexedSeq ContractInList String

data ContractList = ContractList
  { clRetrievedTime :: Maybe UTCTime
  , clContracts :: ContractListISeq
  }
  deriving (Show, Eq)

refreshContracts :: String -> ContractListISeq -> IO (Either String ContractList)
refreshContracts endpoint lOldChain = do
  eresult <- updateContracts endpoint lOldChain
  now <- getCurrentTime
  return $ do contracts <- eresult
              return (ContractList { clRetrievedTime = Just now
                                   , clContracts = contracts })

updateContracts :: String -> ContractListISeq -> IO (Either String ContractListISeq)
updateContracts endpoint oldContracts =
  LazyFeed.foldThroughLazyFeed (completeOldContractList oldContracts) (getAllContracts endpoint)

completeOldContractList :: ContractListISeq -> Maybe ContractInList
                        -> Either ContractListISeq (ContractListISeq -> ContractListISeq)
completeOldContractList oldList (Just h) =
  case ISeq.findMatchingTail h oldList of
    Just matchingTail -> Left matchingTail
    Nothing -> Right $ ISeq.cons h
completeOldContractList _ Nothing = Left ISeq.empty

getAllContracts :: String -> LazyFeed ContractInList
getAllContracts endpoint = getAllContracts' endpoint Start

data Range
  = Start
  | Next ByteString
  | Done
  deriving (Eq, Show)

setRangeHeader :: Range -> Request -> Request
setRangeHeader (Next bs) = setRequestHeader "Range" [bs]
setRangeHeader _ = id

parseRangeHeader :: [ByteString] -> Range
parseRangeHeader [bs] = Next bs
parseRangeHeader _ = Done

getAllContracts' :: String -> Range -> LazyFeed ContractInList
getAllContracts' _endpoint Done = LazyFeed.emptyLazyFeed
getAllContracts' endpoint range = LazyFeed.fromIO $ do
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  mResponse <- try (httpLBS request)
  case mResponse of
    Right response -> do
       case eitherDecode (getResponseBody response) of
         Right (ResultList { results = contracts }) -> do
           let nextRange = parseRangeHeader . getResponseHeader "Next-Range" $ response
           return $ LazyFeed.prependListToLazyFeed contracts (getAllContracts' endpoint nextRange)
         Left str2 -> return $ LazyFeed.errorToLazyFeed $ "Error decoding: " ++ str2
    Left str -> return $ LazyFeed.errorToLazyFeed $ displayException (str :: HttpException)

data ContractListFetchingException = DecodingException String
                                   | RequestException HttpException
  deriving (Show)

instance Exception ContractListFetchingException where
  displayException :: ContractListFetchingException -> String
  displayException (DecodingException msg) = "Decoding exception: " ++ msg
  displayException (RequestException subException) = "Exception querying runtime for contracts: " ++ displayException subException
