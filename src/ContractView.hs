{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ContractView(ContractView(..), contractView) where

import Control.Newtype.Generics (op)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 ( unpack, pack )
import Language.Marlowe.Pretty ( pretty )
import Language.Marlowe.Runtime.Types.ContractJSON (ContractJSON(..), getContractJSON)
import Language.Marlowe.Semantics.Types (Contract, State)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Network.HTTP.Types ( renderSimpleQuery )
import Prelude hiding ( head )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( body, docTypeHtml, h1, head, html, title, b,
                          string, Html, ToMarkup(toMarkup), (!), Markup, code, br, preEscapedString, a, ToValue (toValue) )
import Text.Blaze.Html5.Attributes ( style, lang, href )
import Text.Printf (printf)

import Opts (Options (optRuntimeHost, optRuntimePort), RuntimeHost (..), RuntimePort (..))

baseDoc :: String -> Html -> Html
baseDoc caption content = docTypeHtml
                          $ html ! lang "en"
                                 $ do head $ title $ string caption
                                      body $ do h1 $ string caption
                                                content

contractView :: Options -> Maybe String -> Maybe String -> IO ContractView
contractView _    _   Nothing    = return $ ContractViewError "Need to specify a contractId"
contractView opts tab (Just cid) = do
  let
    rhost = op RuntimeHost . optRuntimeHost $ opts
    rport = op RuntimePort . optRuntimePort $ opts
  v <- getContractJSON (printf "http://%s:%d/" rhost rport) cid
  return $ case v of
    Left str -> ContractViewError str
    Right cjson -> extractInfo (parseTab tab) cjson

parseTab :: Maybe String -> ContractViews
parseTab (Just "state") = CStateView
parseTab _ = CInfoView

extractInfo :: ContractViews -> ContractJSON -> ContractView
extractInfo CInfoView cv =
  ContractInfoView
      (CIVR { civrContractId = CJ.contractId res
            , blockHeaderHash = CJ.blockHeaderHash block
            , blockNo = CJ.blockNo block
            , slotNo = CJ.slotNo block
            , roleTokenMintingPolicyId = CJ.roleTokenMintingPolicyId res
            , status = CJ.status res
            , version = CJ.version res
            })
  where res = CJ.resource cv
        block = CJ.block res
extractInfo CStateView cv =
  ContractStateView
      (CSVR { csvrContractId = CJ.contractId res
            , currentContract = CJ.currentContract res
            , initialContract = CJ.initialContract res
            , currentState = CJ.state res
            })
  where res = CJ.resource cv

allContractViews :: [ContractViews]
allContractViews = [CInfoView, CStateView]

getNavTab :: ContractViews -> String
getNavTab CInfoView = "info"
getNavTab CStateView = "state"

getNavTitle :: ContractViews -> String
getNavTitle CInfoView = "Details"
getNavTitle CStateView = "Code"

data ContractViews = CInfoView
                   | CStateView
  deriving (Eq)

data ContractView = ContractInfoView CIVR
                  | ContractStateView CSVR
                  | ContractViewError String;

instance ToMarkup ContractView where
  toMarkup :: ContractView -> Markup
  toMarkup (ContractInfoView cvr@(CIVR {civrContractId = cid})) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CInfoView cid $ renderCIVR cvr
  toMarkup (ContractStateView ccsr@(CSVR {csvrContractId = cid})) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CStateView cid $ renderCSVR ccsr
  toMarkup (ContractViewError str) =
    baseDoc "An error occurred" (string ("Error: " ++ str))

data CIVR = CIVR { civrContractId :: String
                 , blockHeaderHash :: String
                 , blockNo :: Integer
                 , slotNo :: Integer
                 , roleTokenMintingPolicyId :: String
                 , status :: String
                 , version :: String
                 }

renderCIVR :: CIVR -> Html
renderCIVR (CIVR { civrContractId = cid
                 , blockHeaderHash = blockHash
                 , blockNo = blockNum
                 , slotNo = slotNum
                 , roleTokenMintingPolicyId = roleMintingPolicyId
                 , status = contractStatus
                 , version = marloweVersion
                 }) =
  table ! style "border: 1px solid black"
        $ do tr (do td $ b "Contract ID"
                    td $ string cid)
             tr (do td $ b "Block Header Hash"
                    td $ string blockHash)
             tr (do td $ b "Block No"
                    td $ string (show blockNum))
             tr (do td $ b "Slot No"
                    td $ string (show slotNum))
             tr (do td $ b "Role Token Minting Policy ID"
                    td $ string roleMintingPolicyId)
             tr (do td $ b "Status"
                    td $ string contractStatus)
             tr (do td $ b "Version"
                    td $ string marloweVersion)

data CSVR = CSVR { csvrContractId :: String
                 , currentContract :: Maybe Contract
                 , initialContract :: Contract
                 , currentState :: Maybe State
                 }

renderCSVR :: CSVR -> Html
renderCSVR (CSVR { csvrContractId = cid
                 , currentContract = cc
                 , initialContract = ic
                 , currentState = cs
                 }) =
  table ! style "border: 1px solid black"
        $ do tr (do td $ b "Contract ID"
                    td $ string cid)
             tr (do td $ b "Current contract"
                    td $ renderMContract cc)
             tr (do td $ b "Current state"
                    td $ renderMState cs)
             tr (do td $ b "Initial contract"
                    td $ renderMContract (Just ic))

renderMState :: Maybe State -> Html
renderMState Nothing = string "Contract closed"
renderMState (Just s) = string (show s)

renderMContract :: Maybe Contract -> Html
renderMContract Nothing = string "Contract closed"
renderMContract (Just c) = code $ stringToHtml $ show $ pretty c

addNavBar :: ContractViews -> String -> Html -> Html
addNavBar cv cid c =
  table ! style "border: 1px solid black"
        $ do tr (do td $ b "Navigation bar"
                    mapM_ (\ccv -> mkNavLink (cv == ccv) cid (getNavTab ccv) (getNavTitle ccv))
                          allContractViews
                    c)

mkNavLink :: Bool -> String -> String -> String -> Html
mkNavLink True _ _ tabTitle =
  td $ string tabTitle
mkNavLink False cid tabName tabTitle =
  td $ a ! href (toValue $ generateLink "contractView" [("tab", tabName), ("contractId", cid)])
         $ string tabTitle

table :: Html -> Html
table = H.table ! style "border: 1px solid black"

tr :: Html -> Html
tr = H.tr

td :: Html -> Html
td = H.td ! style "border: 1px solid black; padding: 5px;"

nbsp :: Html
nbsp = preEscapedString "&nbsp;"

splitLeadingSpaces :: String -> (String, String)
splitLeadingSpaces = span (== ' ')

stringToHtml :: String -> Html
stringToHtml str = mconcat $ map processLine $ lines str
  where
    processLine line = let (spaces, rest) = splitLeadingSpaces line
                       in do mconcat (replicate (length spaces) nbsp)
                             string rest
                             br

generateLink :: String -> [(String, String)] -> String
generateLink path params = path ++ unpack (renderSimpleQuery True (map (bimap Data.ByteString.Char8.pack Data.ByteString.Char8.pack) params))
