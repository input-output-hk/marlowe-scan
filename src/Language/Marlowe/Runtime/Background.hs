{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Background
  ( start )
  where

import Control.Concurrent ( forkIO, threadDelay, myThreadId )

import Explorer.SharedContractCache ( ContractListCache, newContractList, readContractList, writeContractList, ContractList (..) )
import Language.Marlowe.Runtime.ContractCaching (refreshContracts)
import GHC.GHCi.Helpers (flushAll)
import GHC.Conc (ThreadId)
import Control.Exception (Exception(displayException), throwTo)

data BackgroundProcessException = BackgroundProcessException
    deriving Show

instance Exception BackgroundProcessException where
  displayException :: BackgroundProcessException -> String
  displayException BackgroundProcessException = "Exception in background process"

start :: String -> IO ContractListCache
start endpoint = do
  contractListCache <- newContractList
  parentThreadId <- myThreadId
  _ <- forkIO $ run (Just parentThreadId) endpoint contractListCache
  pure contractListCache

run :: Maybe ThreadId -> String -> ContractListCache -> IO ()
run mParentThreadId endpoint contractListCache = do
  ContractList { clContracts = oldChain } <- readContractList contractListCache
  eresult <- refreshContracts endpoint oldChain
  case eresult of
    Left err -> do putStrLn $ "ERROR retrieving contracts: " <> err
                   flushAll
                   maybe (return ()) (`throwTo` BackgroundProcessException) mParentThreadId
    Right newContractListContents -> do
      writeContractList contractListCache newContractListContents
  threadDelay 10_000_000
  run Nothing endpoint contractListCache
