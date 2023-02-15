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

import Explorer.Web.ContractView ( ContractView (..), contractView )
import Opts ( Options (optExplorerPort), ExplorerPort (..) )


startApp :: Options -> IO ()
startApp opts = do
  let eport = op ExplorerPort . optExplorerPort $ opts
  putStrLn $ "Marlowe Explorer server started, available at localhost:"
    <> show eport
  run eport $ app opts

type API
     = "listContracts" :> Get '[HTML] String
  :<|> "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> Get '[HTML] ContractView

app :: Options -> Application
app opts = serve (Proxy :: Proxy API) $ hoistServer (Proxy :: Proxy API) liftIO $
       listContracts
  :<|> contractView opts

listContracts :: IO String
listContracts = return "Under contruction!"
