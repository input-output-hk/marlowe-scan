{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractView
  (ContractView(..), contractView)
  where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, code, p, string, toHtml )
import Text.Blaze.Html5.Attributes ( href, style )

import Explorer.Web.Util
import Language.Marlowe.Pretty ( pretty )
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Language.Marlowe.Runtime.Types.ContractJSON
  ( ContractJSON(..), getContractJSON
  , Transaction(..), Transactions(..), getContractTransactions
  )
import qualified Language.Marlowe.Runtime.Types.Common as Common
import Language.Marlowe.Semantics.Types (Contract, Money, Party, State(..), Token)
import Opts (Options, mkUrlPrefix)


contractView :: Options -> Maybe String -> Maybe String -> IO ContractView

contractView opts tab@(Just "txs") (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  cjs <- getContractJSON urlPrefix cid
  case cjs of
    Left str -> pure $ ContractViewError str
    Right cjson -> do
      let link = CJ.linkUrl . CJ.links $ cjson
      etx <- getContractTransactions urlPrefix link
      pure $ case etx of
        Left str -> ContractViewError str
        Right tx -> extractInfo (parseTab tab) cjson (Just tx)

contractView opts tab@(Just _) (Just cid) = do
  cjs <- getContractJSON (mkUrlPrefix opts) cid
  return $ case cjs of
    Left str -> ContractViewError str
    Right cjson -> extractInfo (parseTab tab) cjson Nothing

contractView opts Nothing cid = contractView opts (Just "info") cid

contractView _opts _tab Nothing = return $ ContractViewError "Need to specify a contractId"


parseTab :: Maybe String -> ContractViews
parseTab (Just "state") = CStateView
parseTab (Just "txs") = CTxView
parseTab _ = CInfoView


extractInfo :: ContractViews -> ContractJSON -> Maybe Transactions -> ContractView

extractInfo CInfoView cv _ =
  ContractInfoView
      (CIVR { civrContractId = CJ.contractId res
            , blockHeaderHash = Common.blockHeaderHash block
            , blockNo = Common.blockNo block
            , slotNo = Common.slotNo block
            , roleTokenMintingPolicyId = CJ.roleTokenMintingPolicyId res
            , status = CJ.status res
            , version = CJ.version res
            })
  where res = CJ.resource cv
        block = CJ.block res

extractInfo CStateView cv _ =
  ContractStateView
      (CSVR { csvrContractId = CJ.contractId res
            , currentContract = CJ.currentContract res
            , initialContract = CJ.initialContract res
            , currentState = CJ.state res
            })
  where res = CJ.resource cv

extractInfo CTxView cv (Just (Transactions txs)) =
  ContractTxView . CTVRs (CJ.contractId . CJ.resource $ cv) . map convertTx $ txs
  where
    convertTx tx = CTVR
      { ctvrLink = CJ.linkUrl . txLink $ tx
      , ctvrBlock = Common.blockNo . txBlock $ tx
      , ctvrSlot = Common.slotNo . txBlock $ tx
      , ctvrContractId = txContractId tx
      , ctvrTransactionId = txTransactionId tx
      }

extractInfo _ _ Nothing = ContractViewError "Something went wrong, unable to display"


allContractViews :: [ContractViews]
allContractViews = [CInfoView, CStateView, CTxView]

getNavTab :: ContractViews -> String
getNavTab CInfoView = "info"
getNavTab CStateView = "state"
getNavTab CTxView = "txs"

getNavTitle :: ContractViews -> String
getNavTitle CInfoView = "Details"
getNavTitle CStateView = "Code"
getNavTitle CTxView = "Transactions"

data ContractViews = CInfoView
                   | CStateView
                   | CTxView
  deriving (Eq)

data ContractView = ContractInfoView CIVR
                  | ContractStateView CSVR
                  | ContractTxView CTVRs
                  | ContractViewError String

instance ToMarkup ContractView where
  toMarkup :: ContractView -> Markup
  toMarkup (ContractInfoView cvr@(CIVR {civrContractId = cid})) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CInfoView cid $ renderCIVR cvr
  toMarkup (ContractStateView ccsr@(CSVR {csvrContractId = cid})) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CStateView cid $ renderCSVR ccsr
  toMarkup (ContractTxView (CTVRs cid ctvrs)) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CTxView cid $ renderCTVRs ctvrs
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
             tr (do td $ b "State"
                    td $ renderMState cs)
             tr (do td $ b "Initial contract"
                    td $ renderMContract (Just ic))

data CTVR = CTVR
  { ctvrLink :: String
  , ctvrBlock :: Integer
  , ctvrSlot :: Integer
  , ctvrContractId :: String
  , ctvrTransactionId :: String
  }
  deriving Show

data CTVRs = CTVRs String [CTVR]


renderCTVRs :: [CTVR] -> Html

renderCTVRs [] = p ! style "color: red" $ string "There are no transactions"

renderCTVRs ctvrs = table ! style "border: 1px solid black" $ do
    tr $ do
      th $ b "Transaction ID"
      th $ b "Block No"
      th $ b "Slot No"
    let makeRow ctvr = do
          tr $ do
            td $ string . ctvrTransactionId $ ctvr
            td $ toHtml . ctvrBlock $ ctvr
            td $ toHtml . ctvrSlot $ ctvr
    forM_ ctvrs makeRow


-- renderMState :: Maybe State -> Html
-- renderMState Nothing = string "Contract closed"
-- renderMState (Just s) = string (show s)

renderMAccounts :: (Map.Map (Party, Token) Money) -> Html
renderMAccounts mapAccounts = table ! style "border: 1px solid black" $ do
  tr $ do
    th $ b "party"
    th $ b "currency"
    -- th $ b "currency symbol"
    -- th $ b "token name"
    th $ b "amount"
  let mkRow ((party, token), money) =
        tr $ do
          td . string . show $ party
          td . string . show $ token
          -- td $ "token name"
          td . string . show $ money
  mapM_ mkRow $ Map.toList mapAccounts

renderMState :: Maybe State -> Html
renderMState Nothing = string "Contract closed"
renderMState (Just st) = table ! style "border: 1px solid black" $ do
  tr (do td $ b "accounts"
         td . renderMAccounts . accounts $ st)
  tr (do td $ b "bound values"
         td . string . show . boundValues $ st)
  tr (do td $ b "choices"
         td . string . show . choices $ st)
  tr (do td $ b "minTime"
         td . string . show . minTime $ st)

renderMContract :: Maybe Contract -> Html
renderMContract Nothing = string "Contract closed"
renderMContract (Just c) = code $ stringToHtml $ show $ pretty c

addNavBar :: ContractViews -> String -> Html -> Html
addNavBar cv cid c =
  table ! style "border: 1px solid black"
        $ do tr (do td $ b $ a ! href "listContracts" $ "Contracts List"
                    td $ b "Navigation bar"
                    mapM_ (\ccv -> mkNavLink (cv == ccv) cid (getNavTab ccv) (getNavTitle ccv))
                          allContractViews
                    c)
