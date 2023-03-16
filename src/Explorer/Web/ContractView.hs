{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractView
  (ContractView(..), contractView)
  where

import Control.Monad (forM_)
import Control.Monad.Extra (whenMaybe)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, code, p, string, toHtml )
import Text.Blaze.Html5.Attributes ( href, style )
import Text.Printf (printf)
import Explorer.Web.Util ( tr, th, td, table, baseDoc, mkNavLink, stringToHtml, prettyPrintAmount, makeLocalDateTime )
import Language.Marlowe.Pretty ( pretty )
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import qualified Language.Marlowe.Runtime.Types.TransactionsJSON as TJ
import Language.Marlowe.Semantics.Types (ChoiceId(..), Contract, Money, POSIXTime(..), Party(..),
                                         State(..), Token(..), ValueId(..))
import Opts (Options, mkUrlPrefix)
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))

contractView :: Options -> Maybe String -> Maybe String -> IO ContractView
contractView opts mTab (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
      tab = parseTab mTab
  r <- runExceptT (do cjson <- ExceptT $ CJ.getContractJSON urlPrefix cid
                      let link = CJ.transactions $ CJ.links cjson
                      txjson <- whenMaybe (tab == CTxView)
                                        $ ExceptT $ TJ.getContractTransactions urlPrefix link
                      return $ extractInfo tab cjson txjson)
  return $ either ContractViewError id r

contractView opts Nothing cid = contractView opts (Just "state") cid
contractView _opts _tab Nothing = return $ ContractViewError "Need to specify a contractId"

parseTab :: Maybe String -> ContractViews
parseTab (Just "state") = CStateView
parseTab (Just "txs") = CTxView
parseTab _ = CInfoView


extractInfo :: ContractViews -> CJ.ContractJSON -> Maybe TJ.Transactions -> ContractView
extractInfo CInfoView cv _ =
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
extractInfo CStateView cv _ =
  ContractStateView
      (CSVR { csvrContractId = CJ.contractId res
            , currentContract = CJ.currentContract res
            , initialContract = CJ.initialContract res
            , currentState = CJ.state res
            })
  where res = CJ.resource cv
extractInfo CTxView cv (Just (TJ.Transactions txs)) =
  ContractTxView . CTVRs (CJ.contractId res) $ map convertTx txs
  where
    res = CJ.resource cv
    convertTx tx = CTVR { ctvrLink = TJ.transaction $ TJ.links tx
                        , ctvrBlock = TJ.blockNo $ TJ.block tRes
                        , ctvrSlot = TJ.slotNo $ TJ.block tRes
                        , ctvrContractId = TJ.contractId tRes
                        , ctvrTransactionId = TJ.transactionId tRes
                        }
      where tRes = TJ.resource tx
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
  table $ do tr $ do td $ b "Contract ID"
                     td $ string cid
             tr $ do td $ b "Block Header Hash"
                     td $ string blockHash
             tr $ do td $ b "Block No"
                     td $ string (show blockNum)
             tr $ do td $ b "Slot No"
                     td $ string (show slotNum)
             tr $ do td $ b "Role Token Minting Policy ID"
                     td $ string roleMintingPolicyId
             tr $ do td $ b "Status"
                     td $ string contractStatus
             tr $ do td $ b "Version"
                     td $ string marloweVersion

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
  table $ do tr $ do td $ b "Contract ID"
                     td $ string cid
             tr $ do td $ b "Current contract"
                     td $ renderMContract cc
             tr $ do td $ b "Current state"
                     td $ renderMState cs
             tr $ do td $ b "Initial contract"
                     td $ renderMContract (Just ic)

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
renderCTVRs ctvrs = table $ do
    tr $ do
      th $ b "Transaction ID"
      th $ b "Block No"
      th $ b "Slot No"
    forM_ ctvrs makeRow
  where makeRow ctvr = do
          tr $ do
            td $ string . ctvrTransactionId $ ctvr
            td $ toHtml . ctvrBlock $ ctvr
            td $ toHtml . ctvrSlot $ ctvr

renderParty :: Party -> String
renderParty (Address ad) = printf "Address: %s" $ unpack ad
renderParty (Role ro) = printf "Role: %s" $ unpack ro

renderMAccounts :: Map (Party, Token) Money -> Html
renderMAccounts mapAccounts = table $ do
  tr $ do
    th $ b "party"
    th $ b "currency (token name)"
    th $ b "amount"
  let mkRow ((party, token), money) =
        let (tokenString, moneyString) = renderToken token money in
        tr $ do
          td . string . renderParty $ party
          td . string $ tokenString
          td . string $ moneyString
  mapM_ mkRow $ Map.toList mapAccounts

renderToken :: Token -> Money -> (String, String)
renderToken (Token "" "") money = ("ADA", prettyPrintAmount 6 money)
renderToken (Token currSymbol tokenName) money = (printf "%s (%s)" currSymbol tokenName, prettyPrintAmount 0 money)

renderBoundValues :: Map ValueId Integer -> String
renderBoundValues mapBoundValues = case Map.toList mapBoundValues of
  [] -> "-"
  listBoundValues -> intercalate ", "
    . map (\(ValueId vid, int) -> show vid <> ": " <> show int)
    $ listBoundValues

renderChoices :: Map ChoiceId a -> String
renderChoices mapChoices = case Map.keys mapChoices of
  [] -> "-"
  listChoiceIds -> intercalate ", "
    . map (\(ChoiceId choiceName party) -> show party <> ": " <> unpack choiceName)
    $ listChoiceIds

renderTime :: POSIXTime -> Html
renderTime =
  makeLocalDateTime  -- ..and format it.
  . posixSecondsToUTCTime  -- ..convert to UTCTime for the formatting function..
  . realToFrac . (/ (1000 :: Double)) . fromIntegral  -- ..convert from millis to epoch seconds..
  . getPOSIXTime  -- Get the Integer out of our custom type..

renderMState :: Maybe State -> Html
renderMState Nothing = string "Contract closed"
renderMState (Just (State { accounts    = accs
                          , choices     = chos
                          , boundValues = boundVals
                          , minTime     = mtime })) =
  table $ do tr $ do td $ b "accounts"
                     td $ renderMAccounts accs
             tr $ do td $ b "bound values"
                     td $ string $ renderBoundValues boundVals
             tr $ do td $ b "choices"
                     td $ string $ renderChoices chos
             tr $ do td $ b "minTime"
                     td $ do renderTime mtime
                             string $ " (POSIX: " ++ show mtime ++ ")"

renderMContract :: Maybe Contract -> Html
renderMContract Nothing = string "Contract closed"
renderMContract (Just c) = code $ stringToHtml $ show $ pretty c

addNavBar :: ContractViews -> String -> Html -> Html
addNavBar cv cid c =
  table $ do tr $ do td $ b $ a ! href "listContracts" $ "Contracts List"
                     td $ b "Navigation bar"
                     mapM_ (\ccv -> mkNavLink (cv == ccv) cid (getNavTab ccv) (getNavTitle ccv))
                           allContractViews
                     c
