{-# LANGUAGE NumericUnderscores #-}

module Language.Marlowe.Runtime.Background
  ( start )
  where

import Control.Concurrent ( forkIO, threadDelay )

import Control.Concurrent.Var ( Var, modifyVar_, newVar, readVar )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList(..), getContracts )


start :: String -> IO (Var ContractList)
start endpoint = do
  varContractList <- newVar $ ContractList Nothing []
  -- Bad to discard the ThreadId here? I was thinking this thread will die when its parent (the Servant server) dies. It seems to!
  _ <- forkIO $ run endpoint varContractList
  pure varContractList

run :: String -> Var ContractList -> IO ()
run endpoint varContractList = do
  ContractList _ oldChain <- readVar varContractList
  eresult <- getContracts endpoint oldChain
  case eresult of
    Left err -> putStrLn $ "ERROR retrieving contracts: " <> err
    Right newContractList -> do
      modifyVar_ varContractList (pure . const newContractList)
  threadDelay 10_000_000
  run endpoint varContractList

