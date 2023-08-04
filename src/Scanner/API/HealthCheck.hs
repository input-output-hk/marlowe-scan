{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MonoLocalBinds #-}

module Scanner.API.HealthCheck(HealthCheckResult(..), healthCheck) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Scanner.Web.Util (SyncStatus(..))
import Scanner.SharedContractCache (ContractListCacheStatusReader (..))

data HealthCheckResult = HealthCheckResult
  { alive :: Bool
  , ready :: Bool
  , healthy :: Bool
  , millisecondsSinceLastUpdate :: Maybe Integer
  } deriving (Show, Eq, Generic, ToJSON)

healthCheck :: ContractListCacheStatusReader cache => cache -> IO HealthCheckResult
healthCheck cache = do
    curSyncStatus <- getSyncStatus cache
    pure $ case curSyncStatus of
              Syncing -> HealthCheckResult
                                { alive = True
                                , ready = False
                                , healthy = True
                                , millisecondsSinceLastUpdate = Nothing
                                }
              Synced ndt _ -> HealthCheckResult
                                      { alive = True
                                      , ready = True
                                      , healthy = True
                                      , millisecondsSinceLastUpdate = Just $ round $ 1000 * ndt
                                      }
              OutOfSync ndt _ -> HealthCheckResult
                                      { alive = True
                                      , ready = True
                                      , healthy = False
                                      , millisecondsSinceLastUpdate = Just $ round $ 1000 * ndt
                                      }