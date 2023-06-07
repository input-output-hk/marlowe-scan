{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Explorer.SharedContractCache
  ( ContractList(..)
  , ContractListCacheReader(..)
  , ContractListCacheStatusReader(..)
  , ContractListCache
  , newContractList
  , writeContractList
  )
  where

import Control.Concurrent.MVar ( MVar, modifyMVar_, newMVar, readMVar )
import Data.Time (UTCTime)
import Language.Marlowe.Runtime.Types.ContractsJSON (ContractListISeq)
import Data.Time.Clock (getCurrentTime)
import Explorer.Web.Util (SyncStatus, calculateSyncStatus)
import Language.Marlowe.Runtime.Types.IndexedSeq (empty)

data InnerContractList = InnerContractList
  { iclRetrievedTime :: Maybe UTCTime
  , iclContracts :: ContractListISeq
  }
  deriving (Show, Eq)

newtype ContractListCache = ContractListCache (MVar InnerContractList)

newContractList :: IO ContractListCache
newContractList = ContractListCache <$> newMVar (InnerContractList { iclRetrievedTime = Nothing
                                                                   , iclContracts = empty
                                                                   })

writeContractList :: ContractListCache -> ContractListISeq -> IO ()
writeContractList (ContractListCache contractListCache) contractList = do
  curTime <- getCurrentTime
  modifyMVar_ contractListCache (pure . const (InnerContractList { iclRetrievedTime = Just curTime
                                                                 , iclContracts = contractList
                                                                 }))

data ContractList = ContractList
  { clSyncStatus :: SyncStatus
  , clContracts :: ContractListISeq
  }
  deriving (Show, Eq)

class ContractListCacheReader a where
  readContractList :: a -> IO ContractList

instance ContractListCacheReader ContractListCache where
  readContractList :: ContractListCache -> IO ContractList
  readContractList (ContractListCache contractListCache) = do
    InnerContractList { iclRetrievedTime = mRetrievedTime
                      , iclContracts = mContractSeq
                      } <- readMVar contractListCache
    curTime <- getCurrentTime
    return $ ContractList { clSyncStatus = calculateSyncStatus curTime mRetrievedTime
                          , clContracts = mContractSeq
                          }

class ContractListCacheStatusReader a where
  getSyncStatus :: a -> IO SyncStatus

instance ContractListCacheReader a => ContractListCacheStatusReader a where
  getSyncStatus :: a -> IO SyncStatus
  getSyncStatus contractListCache = do
    ContractList{ clSyncStatus = mRetrievedTime } <- readContractList contractListCache
    return mRetrievedTime


