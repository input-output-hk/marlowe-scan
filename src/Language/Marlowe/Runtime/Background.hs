{-# LANGUAGE NumericUnderscores #-}

module Language.Marlowe.Runtime.Background
  ( start )
  where

import Control.Concurrent ( forkIO, threadDelay )

import Explorer.SharedContractCache
  ( ContractListCache
  , newContractList
  , readContractList
  , writeContractList
  )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList(..), refreshContracts )
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq

start :: String -> IO ContractListCache
start endpoint = do
  contractListCache <- newContractList $ ContractList Nothing ISeq.empty
  _ <- forkIO $ run endpoint contractListCache
  pure contractListCache

run :: String -> ContractListCache -> IO ()
run endpoint contractListCache = do
  ContractList _ oldChain <- readContractList contractListCache
  eresult <- refreshContracts endpoint oldChain
  case eresult of
    Left err -> putStrLn $ "ERROR retrieving contracts: " <> err
    Right newContractListContents -> do
      writeContractList contractListCache newContractListContents
  threadDelay 10_000_000
  run endpoint contractListCache
