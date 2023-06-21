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
import Servant ( Proxy(..), hoistServer, serve, type (:<|>)(..), OctetStream, QueryParam, type (:>), Get, Headers, Header, JSON, HasServer (ServerT) )

import Explorer.SharedContractCache ( ContractListCacheReader )
import Explorer.Web.ContractListView ( ContractListView (..), contractListView )
import Explorer.Web.ContractView ( ContractView (..), contractView )
import Language.Marlowe.Runtime.Background ( start )
import Opts ( Options (optExplorerPort), ExplorerPort (..), mkUrlPrefix )
import Explorer.Web.ContractInfoDownload (contractDownloadInfo)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import Explorer.API.GetNumTransactions (getContractNumTransactions)
import Explorer.API.IsContractOpen (isContractOpen)
import Explorer.API.HealthCheck (HealthCheckResult, healthCheck)
import Explorer.Resources.Data (cssStylesheet, activeLight, greenStatus, inactiveLight, logo, magnifyingGlass, amberStatus, redStatus, downloadIcon, blockHeaderHashIcon, blockNoIcon, contractIdIcon, metadataIcon, roleTokenMintingPolicyIdIcon, slotNoIcon, statusIcon, versionIcon, prismJS, prismCSS, marlowePrismJS, marlowePrismCSS, stateIcon, arrowDropDown, alarmClock)
import Explorer.Resources.MimeTypes (CSS, SVG, JS)
import Servant.HTML.Blaze (HTML)

startApp :: Options -> IO ()
startApp opts = do
  let eport = op ExplorerPort . optExplorerPort $ opts
  putStrLn $ "Marlowe Explorer server started, available at localhost:"
    <> show eport
  contractListCache <- start $ mkUrlPrefix opts
  run eport $ app opts contractListCache

type CSSResourcesAPI = "css" :> "stylesheet.css" :> Get '[CSS] BS.ByteString

type SVGResourcesAPI = "svg" :> (("active-light.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("alarm_clock.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("amber-status-light.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("green-status-light.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("inactive-light.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("logo.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("magnifying-glass.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("red-status-light.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("download.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("arrow_drop_down.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("block_header_hash.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("block_no.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("contract_id.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("metadata.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("role_token_minting_policy_id.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("slot_no.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("state.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("status.svg" :> Get '[SVG] BS.ByteString)
                            :<|> ("version.svg" :> Get '[SVG] BS.ByteString))

type PrismResourcesAPI = "prism" :> ("prism.js" :> Get '[JS] BS.ByteString
                                :<|> "prism.css" :> Get '[CSS] BS.ByteString
                                :<|> "marlowe.js" :> Get '[CSS] BS.ByteString
                                :<|> "marlowe.css" :> Get '[CSS] BS.ByteString)

type ResourcesAPI = CSSResourcesAPI
                :<|> SVGResourcesAPI
                :<|> PrismResourcesAPI

cssResources :: ServerT CSSResourcesAPI IO
cssResources = return cssStylesheet

svgResources :: ServerT SVGResourcesAPI IO
svgResources = return activeLight
          :<|> return alarmClock
          :<|> return amberStatus
          :<|> return greenStatus
          :<|> return inactiveLight
          :<|> return logo
          :<|> return magnifyingGlass
          :<|> return redStatus
          :<|> return downloadIcon
          :<|> return arrowDropDown
          :<|> return blockHeaderHashIcon
          :<|> return blockNoIcon
          :<|> return contractIdIcon
          :<|> return metadataIcon
          :<|> return roleTokenMintingPolicyIdIcon
          :<|> return slotNoIcon
          :<|> return stateIcon
          :<|> return statusIcon
          :<|> return versionIcon

prismResources :: ServerT PrismResourcesAPI IO
prismResources = return prismJS
            :<|> return prismCSS
            :<|> return marlowePrismJS
            :<|> return marlowePrismCSS

appResources :: ServerT ResourcesAPI IO
appResources = cssResources :<|> svgResources :<|> prismResources

type API
     = Get '[HTML] ContractListView  -- Initial "index" page, http://HOST:PORT/
  :<|> "listContracts" :> QueryParam "page" Int :> Get '[HTML] ContractListView
  :<|> "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String
                      :> QueryParam "page" Int :> QueryParam "transactionId" String
                      :> Get '[HTML] ContractView
  :<|> "contractDownloadInfo" :> QueryParam "contractId" String :> Get '[OctetStream] (Headers '[Header "Content-Disposition" String] ByteString)
  :<|> "isContractOpen" :> QueryParam "contractId" String :> Get '[JSON] Bool
  :<|> "getNumTransactions" :> QueryParam "contractId" String :> Get '[JSON] Integer
  :<|> "health" :> Get '[JSON] HealthCheckResult
  :<|> ResourcesAPI

app :: ContractListCacheReader contractListCache => Options -> contractListCache -> Application
app opts contractListCache =
  serve (Proxy :: Proxy API) $
    hoistServer (Proxy :: Proxy API) liftIO
    (contractListView opts contractListCache Nothing
    :<|> contractListView opts contractListCache
    :<|> contractView opts contractListCache
    :<|> contractDownloadInfo opts
    :<|> isContractOpen opts
    :<|> getContractNumTransactions opts
    :<|> healthCheck contractListCache
    :<|> appResources)
