{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , app
  ) where

import Control.Monad.IO.Class ( liftIO )
import Control.Newtype.Generics ( op )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( serve, Proxy(..), type (:>), Get, type (:<|>) ((:<|>)), QueryParam )
import Servant.HTML.Blaze ( HTML )
import Servant.Server ( hoistServer )

import Control.Concurrent.Var ( Var )
import Explorer.Web.ContractListView ( ContractListView (..), contractListView )
import Explorer.Web.ContractView ( ContractView (..), contractView )
import Language.Marlowe.Runtime.Background ( start )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList )
import Opts ( Options (optExplorerPort), ExplorerPort (..), mkUrlPrefix )


startApp :: Options -> IO ()
startApp opts = do
  let eport = op ExplorerPort . optExplorerPort $ opts
  putStrLn $ "Marlowe Explorer server started, available at localhost:"
    <> show eport
  varContractList <- start $ mkUrlPrefix opts
  run eport $ app opts varContractList

type API
     = Get '[HTML] ContractListView  -- Initial "index" page, http://HOST:PORT/
  :<|> "listContracts" :> Get '[HTML] ContractListView
  :<|> "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> Get '[HTML] ContractView

app :: Options -> Var ContractList-> Application
app opts varContractList = serve (Proxy :: Proxy API) $ hoistServer (Proxy :: Proxy API) liftIO $
       contractListView opts varContractList
  :<|> contractListView opts varContractList
  :<|> contractView opts
