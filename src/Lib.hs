{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import ContractView ( ContractView(..), contractView )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( serve, Proxy(..), type (:>), Get, type (:<|>) ((:<|>)), QueryParam )
import Servant.HTML.Blaze ( HTML )
import Servant.Server ( hoistServer )
import Control.Monad.IO.Class (liftIO)

startApp :: IO ()
startApp = run 8080 app

type API = "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> Get '[HTML] ContractView
      :<|> "listContracts" :> Get '[HTML] String

app :: Application
app = serve (Proxy :: Proxy API) (hoistServer (Proxy :: Proxy API) liftIO (contractView :<|> listContracts))

listContracts :: IO String
listContracts = return "Under contruction!"
