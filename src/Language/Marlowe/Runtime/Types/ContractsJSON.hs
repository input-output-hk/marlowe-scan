{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractList(..)
  , ContractInList(..)
  , Resource(..)
  , getContracts
  )
  where

import Control.Exception ( try )
import Control.Monad.Except ( ExceptT, runExceptT, throwError )
import Control.Monad.Reader ( ReaderT, asks, runReaderT )
import Control.Monad.Trans ( liftIO )
import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode )
import Data.ByteString ( ByteString )
import Data.List ( foldl' )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Network.HTTP.Simple ( HttpException, Request, parseRequest,
  getResponseBody, httpLBS, getResponseHeader, setRequestHeader,
  setRequestMethod )

import Language.Marlowe.Runtime.AdjustTip ( BuildChain (Building, BuildDone),
  Seq, (><), empty, fromList, processTip, toList )
import Language.Marlowe.Runtime.Types.Common ( Block, Link(..) )


data Range
  = Start
  | Next ByteString
  | Done
  deriving (Eq, Show)

data ContractList = ContractList
  { clRetrievedTime :: UTCTime
  , clContracts :: [ContractInList]
  }
  deriving (Eq, Show)

data ContractInList = ContractInList
  { cilLink :: Link
  , cilResource :: Resource
  }
  deriving (Eq, Show)

instance FromJSON ContractInList where
  parseJSON = withObject "ContractInList" $ \o -> ContractInList
    <$> (Link <$> (o .: "links" >>= (.: "contract")))
    <*> o .: "resource"


-- There's probably a simpler way to do this without an internal "dummy"
-- newtype wrapper. I had trouble understanding how to parse into a simple
-- [ContractInList] and also drill down into the "results" part of the JSON
-- document at the same time.
-- And writing instances for [ContractInList] in various ways kept leading me
-- to overlapping instance errors.

newtype InternalContractList = InternalContractList [ContractInList]

instance FromJSON InternalContractList where
  parseJSON = withObject "InternalContractList" $ \o -> do
    InternalContractList <$> o .: "results"


data Resource = Resource
  { resContractId :: String
  , resBlock :: Block
  , resRoleTokenMintingPolicyId :: String
  }
  deriving (Eq, Show)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o -> Resource
    <$> o .: "contractId"
    <*> o .: "block"
    <*> o .: "roleTokenMintingPolicyId"


type GetContracts a = ReaderT (String, Seq ContractInList) (ExceptT String IO) a

runGetContracts :: (String, Seq ContractInList) -> GetContracts a -> IO (Either String a)
runGetContracts env ev = runExceptT $ runReaderT ev env


getContracts :: String -> [ContractInList] -> IO (Either String ContractList)
getContracts endpoint lOldChain = do
  eresult <- runGetContracts (endpoint, fromList lOldChain) $ getContracts' (empty, Start)
  now <- getCurrentTime
  return $ (Right . ContractList now . toList . fst) =<< eresult


-- This code has two criteria when trying to get the latest Marlowe contract
-- data to add to what we already have cached.
--
-- 1. We need to keep asking the Marlowe Runtime for pages of contract data
--    until the Next-Range header indicates there are no more.
-- 2. Regardless of the Runtime paging above, we need to only download
--    contracts from the tip backwards until we locate the point where our
--    prior contract list has them already. This will handle both new contracts
--    since the last time and also any rollbacks that have occurred, because we
--    trim off the cached chain prior to the point where it matches new data.
--
-- Point 1 is what the Range (Start | Next ... | Done) data structure is for
-- Point 2 is what the Language.Marlowe.Runtime.AdjustTip.BuildChain
-- (Building | BuildDone) data structure is for

getContracts' :: (Seq ContractInList, Range) -> GetContracts (Seq ContractInList, Range)

getContracts' t@(_acc, Done) = return t

getContracts' (acc, range) = do
  (nextContracts, nextRange) <- contractsRESTCall range
  oldChain <- asks snd
  case processTip oldChain nextContracts of
    -- Note setting Range to Done here because we have enough info and can stop
    BuildDone endOfChain -> getContracts' (acc >< endOfChain, Done)
    Building newTip -> getContracts' (acc >< newTip, nextRange)


setRangeHeader :: Range -> Request -> Request
setRangeHeader (Next bs) = setRequestHeader "Range" [bs]
setRangeHeader _ = id

parseRangeHeader :: [ByteString] -> Range
parseRangeHeader [bs] = Next bs
parseRangeHeader _ = Done

contractsRESTCall :: Range -> GetContracts (Seq ContractInList, Range)
contractsRESTCall range = do
  endpoint <- asks fst
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  eresponse <- liftIO . try $ httpLBS request
  case eresponse of
    Left exception -> throwError . show $ (exception :: HttpException)
    Right response -> case eitherDecode (getResponseBody response) of
      Left err -> throwError err
      Right (InternalContractList contracts) -> do
        let nextRange = parseRangeHeader . getResponseHeader "Next-Range" $ response
        return (fromList contracts, nextRange)
