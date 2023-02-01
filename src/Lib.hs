{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import ContractView ( ContractView(..), contractView )
import Control.Monad.IO.Class (liftIO)
import Control.Newtype.Generics (op)
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( serve, Proxy(..), type (:>), Get, type (:<|>) ((:<|>)), QueryParam )
import Servant.HTML.Blaze ( HTML )
import Servant.Server ( hoistServer )

import Opts (Options (optExplorerPort), ExplorerPort (..))

startApp :: Options -> IO ()
startApp opts = run (op ExplorerPort . optExplorerPort $ opts) $ app opts

type API = "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> Get '[HTML] ContractView
      :<|> "listContracts" :> Get '[HTML] String

app :: Options -> Application
app opts = serve (Proxy :: Proxy API) (hoistServer (Proxy :: Proxy API) liftIO (contractView opts :<|> listContracts))

listContracts :: IO String
listContracts = return "Under contruction!"
