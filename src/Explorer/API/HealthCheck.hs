{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Explorer.API.HealthCheck(HealthCheckResult(..), healthCheck) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Explorer.SharedContractCache (ContractListCache, readContractList)
import Language.Marlowe.Runtime.Types.ContractsJSON (ContractList(..))
import Data.Time (getCurrentTime, diffUTCTime)

data HealthCheckResult = HealthCheckResult
  { alive :: Bool
  , ready :: Bool
  , healthy :: Bool
  , millisecondsSinceLastUpdate :: Maybe Integer
  } deriving (Show, Eq, Generic, ToJSON)

healthCheck :: ContractListCache -> IO HealthCheckResult
healthCheck cache = do
    ContractList { clRetrievedTime = mRetrievedTime } <- readContractList cache
    now <- getCurrentTime
    pure $ case mRetrievedTime of
            Nothing -> HealthCheckResult
                         { alive = True
                         , ready = False
                         , healthy = True
                         , millisecondsSinceLastUpdate = Nothing
                         }
            Just lastUpdated ->
               let millisSinceLastUpdated = round (1000 * (now `diffUTCTime` lastUpdated)) in
               HealthCheckResult
                 { alive = True
                 , ready = True
                 , healthy = millisSinceLastUpdated < 60000 -- Healthy if it was updated in the last minute
                 , millisecondsSinceLastUpdate = Just millisSinceLastUpdated
                 }
