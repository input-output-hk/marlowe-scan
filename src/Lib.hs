{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  ) where

import Control.Monad.IO.Class ( liftIO )
import Control.Newtype.Generics ( op )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( Proxy(..), hoistServer, serve, type (:<|>)(..), OctetStream, QueryParam, type (:>), Get, Headers, Header, JSON )
import Servant.HTML.Blaze ( HTML )

import Explorer.SharedContractCache ( ContractListCache )
import Explorer.Web.ContractListView ( ContractListView (..), contractListView )
import Explorer.Web.ContractView ( ContractView (..), contractView )
import Language.Marlowe.Runtime.Background ( start )
import Opts ( Options (optExplorerPort), ExplorerPort (..), mkUrlPrefix )
import Explorer.Web.ContractInfoDownload (contractDownloadInfo)
import Data.ByteString.Lazy (ByteString)
import Explorer.API.GetNumTransactions (getContractNumTransactions)
import Explorer.API.IsContractOpen (isContractOpen)

startApp :: Options -> IO ()
startApp opts = do
  let eport = op ExplorerPort . optExplorerPort $ opts
  putStrLn $ "Marlowe Explorer server started, available at localhost:"
    <> show eport
  contractListCache <- start $ mkUrlPrefix opts
  run eport $ app opts contractListCache

type API
     = Get '[HTML] ContractListView  -- Initial "index" page, http://HOST:PORT/
  :<|> "listContracts" :> QueryParam "page" Int :> Get '[HTML] ContractListView
  :<|> "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> QueryParam "transactionId" String :> Get '[HTML] ContractView
  :<|> "contractDownloadInfo" :> QueryParam "contractId" String :> Get '[OctetStream] (Headers '[Header "Content-Disposition" String] ByteString)
  :<|> "isContractOpen" :> QueryParam "contractId" String :> Get '[JSON] Bool
  :<|> "getNumTransactions" :> QueryParam "contractId" String :> Get '[JSON] Integer

app :: Options -> ContractListCache -> Application
app opts contractListCache =
  serve (Proxy :: Proxy API) $
    hoistServer (Proxy :: Proxy API) liftIO
    (contractListView opts contractListCache Nothing
    :<|> contractListView opts contractListCache
    :<|> contractView opts
    :<|> contractDownloadInfo opts
    :<|> isContractOpen opts
    :<|> getContractNumTransactions opts)


