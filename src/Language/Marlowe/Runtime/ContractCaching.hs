{-# LANGUAGE OverloadedStrings #-}
module Language.Marlowe.Runtime.ContractCaching (refreshContracts) where

import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList(..), ContractListISeq, ResultList(ResultList, results), ContractInList )
import qualified Language.Marlowe.Runtime.Types.LazyFeed as LazyFeed
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq
import Language.Marlowe.Runtime.Types.LazyFeed (LazyFeed)
import Network.HTTP.Simple (HttpException, parseRequest, setRequestHeader, setRequestMethod, httpLBS, getResponseBody, getResponseHeader)
import Data.Time.Clock (getCurrentTime)
import Data.Foldable (foldl')
import Control.Exception (try, Exception (displayException))
import Data.Aeson (eitherDecode)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Error.Util (hoistEither)
import Data.Either.Extra (mapLeft)
import Language.Marlowe.Runtime.Types.General (Range (..), setRangeHeader, parseRangeHeader)

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

getAllContracts' :: String -> Range -> LazyFeed ContractInList
getAllContracts' _endpoint Done = LazyFeed.emptyLazyFeed
getAllContracts' endpoint range = LazyFeed.fromExceptTIO $ do
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  response <- ExceptT $ httpExceptionToString <$> try (httpLBS request)
  (ResultList { results = contracts }) <- hoistEither $ eitherDecode (getResponseBody response)
  let nextRange = parseRangeHeader $ getResponseHeader "Next-Range" response
  return $ LazyFeed.prependListToLazyFeed contracts (getAllContracts' endpoint nextRange)
    
  where
    httpExceptionToString :: Either HttpException a -> Either String a
    httpExceptionToString = mapLeft displayException
