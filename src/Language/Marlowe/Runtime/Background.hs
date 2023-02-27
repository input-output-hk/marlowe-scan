{-# LANGUAGE NumericUnderscores #-}

module Language.Marlowe.Runtime.Background
  ( start )
  where

import Control.Concurrent ( forkIO, threadDelay )
import Data.Time.Clock ( secondsToNominalDiffTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import Control.Concurrent.Var ( Var, modifyVar_, newVar )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList(..), getContracts )


start :: String -> IO (Var ContractList)
start endpoint = do
  let bogusTime = posixSecondsToUTCTime . secondsToNominalDiffTime $ 0
  varContractList <- newVar $ ContractList bogusTime []
  -- Bad to discard the ThreadId here? I was thinking this thread will die when its parent (the Servant server) dies. It seems to!
  _ <- forkIO $ run endpoint varContractList
  pure varContractList

run :: String -> Var ContractList -> IO ()
run endpoint varContractList = do
  eresult <- getContracts endpoint
  case eresult of
    Left err -> putStrLn $ "ERROR retrieving contracts: " <> err
    Right newContractList -> do
      modifyVar_ varContractList (pure . const newContractList)
  threadDelay 10_000_000
  run endpoint varContractList

