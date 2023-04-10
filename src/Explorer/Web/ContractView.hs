{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractView
  (ContractView(..), contractView)
  where

import Control.Monad (forM_, forM)
import Control.Monad.Extra (whenMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Utils.Misc (split)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, code, p, string, ToValue (toValue) )
import Text.Blaze.Html5.Attributes ( href, style )
import Text.Printf (printf)
import Explorer.Web.Util ( tr, th, td, table, baseDoc, stringToHtml, prettyPrintAmount, makeLocalDateTime, generateLink, mkTransactionExplorerLink, mkBlockExplorerLink, mkTokenPolicyExplorerLink  )
import Language.Marlowe.Pretty ( pretty )
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import qualified Language.Marlowe.Runtime.Types.TransactionsJSON as TJs
import qualified Language.Marlowe.Runtime.Types.TransactionJSON as TJ
import Language.Marlowe.Semantics.Types (ChoiceId(..), Contract, Input(..), Money, POSIXTime(..), Party(..),
                                         State(..), Token(..), ValueId(..))
import Opts (Options (optBlockExplorerHost, Options), mkUrlPrefix, BlockExplorerHost (BlockExplorerHost))
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Prelude hiding (div)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Data.Map as M

contractView :: Options -> Maybe String -> Maybe String -> Maybe String -> IO ContractView
contractView opts@(Options {optBlockExplorerHost = BlockExplorerHost blockExplHost}) mTab (Just cid) mTxId = do
  let urlPrefix = mkUrlPrefix opts
      tab = parseTab mTab
  r <- runExceptT (do cjson <- ExceptT $ CJ.getContractJSON urlPrefix cid
                      let link = CJ.transactions $ CJ.links cjson
                      txsjson <- whenMaybe (tab == CTxView)
                                           $ ExceptT $ TJs.getContractTransactionsByLink urlPrefix link
                      let mTxId2 = getIfInContractDefaultFirst mTxId txsjson
                      txjson <- forM mTxId2 (ExceptT . TJ.getTransaction urlPrefix link)
                      return $ extractInfo tab blockExplHost cjson txsjson txjson)
  return $ either ContractViewError id r
contractView opts Nothing cid txId = contractView opts (Just "state") cid txId
contractView _opts _tab Nothing _txId = return $ ContractViewError "Need to specify a contractId"


getIfInContractDefaultFirst :: Maybe String -> Maybe TJs.Transactions -> Maybe String
getIfInContractDefaultFirst mTxId@(Just txId) (Just (TJs.Transactions { TJs.transactions = txList })) =
  if any ((== txId) . TJs.transactionId . TJs.resource) txList then mTxId else Nothing
getIfInContractDefaultFirst Nothing (Just (TJs.Transactions { TJs.transactions = tns@(_:_) } )) =
  Just (TJs.transactionId $ TJs.resource $ last tns)
getIfInContractDefaultFirst _ _ = Nothing


parseTab :: Maybe String -> ContractViews
parseTab (Just "state") = CStateView
parseTab (Just "txs") = CTxView
parseTab _ = CInfoView


extractInfo :: ContractViews -> String -> CJ.ContractJSON -> Maybe TJs.Transactions -> Maybe TJ.Transaction -> ContractView
extractInfo CInfoView blockExplHost CJ.ContractJSON { CJ.resource = 
                                         (CJ.Resource { CJ.block = CJ.Block { CJ.blockHeaderHash = blkHash
                                                                            , CJ.blockNo = blkNo
                                                                            , CJ.slotNo = sltNo
                                                                            }
                                                      , CJ.contractId = cid
                                                      , CJ.metadata =_metadata
                                                      , CJ.roleTokenMintingPolicyId = mintingPolicyId
                                                      , CJ.status = currStatus
                                                      , CJ.tags = tagsMap
                                                      , CJ.version = ver
                                                      })
                                                     } _ _ =
  ContractInfoView
      (CIVR { civrContractId = cid
            , civrContractIdLink = mkTransactionExplorerLink blockExplHost cid
            , civrBlockHeaderHash = blkHash
            , civrBlockNo = blkNo
            , civrBlockLink = mkBlockExplorerLink blockExplHost blkNo
            , civrSlotNo = sltNo
            , civrRoleTokenMintingPolicyId = mintingPolicyId
            , civrRoleTokenMintingPolicyIdLink = mkTokenPolicyExplorerLink blockExplHost mintingPolicyId
            , civrTags = tagsMap
            , civrStatus = currStatus
            , civrVersion = ver
            })
extractInfo CStateView blockExplHost CJ.ContractJSON { CJ.resource = 
                                         (CJ.Resource { CJ.contractId = cid
                                                      , CJ.currentContract = currContract
                                                      , CJ.initialContract = initContract
                                                      , CJ.state = currState
                                                      })
                                                     } _ _ =
  ContractStateView
      (CSVR { csvrContractId = cid
            , csvrContractIdLink = mkTransactionExplorerLink  blockExplHost cid
            , currentContract = currContract
            , initialContract = initContract
            , currentState = currState
            , csvrBlockExplHost = blockExplHost
            })
extractInfo CTxView blockExplHost CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.contractId = cid }
                                                  }
            (Just (TJs.Transactions { TJs.transactions = txs })) mTx =
  ContractTxView $ CTVRs { ctvrsContractId = cid
                         , ctvrs = map convertTx $ reverse txs
                         , ctvrsSelectedTransactionInfo = fmap (convertTxDetails blockExplHost) mTx
                         , ctvrsBlockExplHost = blockExplHost
                         }
  where
    convertTx TJs.Transaction { TJs.resource = TJs.Resource { TJs.block = TJs.Block { TJs.blockNo = blockNo'
                                                                                    , TJs.slotNo = slotNo'
                                                                                    }
                                                            , TJs.contractId = txContractId
                                                            , TJs.transactionId = transactionId'
                                                            }
                              } =
                   CTVR { ctvrBlock = blockNo'
                        , ctvrBlockLink = mkBlockExplorerLink blockExplHost blockNo'
                        , ctvrSlot = slotNo'
                        , ctvrContractId = txContractId
                        , ctvrTransactionId = transactionId'
                        }
extractInfo _ _blockExplHost _ Nothing _ = ContractViewError "Something went wrong, unable to display"

convertTxDetails :: String -> TJ.Transaction -> CTVRTDetail
convertTxDetails blockExplHost TJ.Transaction { TJ.links = TJ.Link { TJ.next = mNext
                                                                   , TJ.previous = mPrev
                                                                   }
                                              , TJ.resource = TJ.Resource { TJ.block = TJ.Block { TJ.blockHeaderHash = txDetailBlockHeaderHash
                                                                                                , TJ.blockNo = txDetailBlockNo
                                                                                                , TJ.slotNo = txDetailSlotNo
                                                                                                }
                                                                          , TJ.inputs = txDetailInputs
                                                                          , TJ.invalidBefore = txDetailInvalidBefore
                                                                          , TJ.invalidHereafter = txDetailInvalidHereafter
                                                                          , TJ.outputContract = txDetailOutputContract
                                                                          , TJ.outputState = txDetailOutputState
                                                                          , TJ.status = txDetailStatus
                                                                          , TJ.tags = txDetailTags
                                                                          , TJ.transactionId = txDetailTransactionId
                                                                          }
                                              }
  = CTVRTDetail { txPrev= mPrev
                , txNext= mNext
                , txBlockHeaderHash = txDetailBlockHeaderHash
                , txBlockNo = txDetailBlockNo
                , txBlockLink = mkBlockExplorerLink blockExplHost txDetailBlockNo
                , txSlotNo = txDetailSlotNo
                , inputs = txDetailInputs
                , invalidBefore = txDetailInvalidBefore
                , invalidHereafter = txDetailInvalidHereafter
                , outputContract = txDetailOutputContract
                , outputState = txDetailOutputState
                , txStatus = txDetailStatus
                , txTags = txDetailTags
                , transactionId = txDetailTransactionId
                }


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
  deriving (Show, Eq)

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
  toMarkup (ContractTxView ctvrs'@CTVRs {ctvrsContractId = cid}) =
    baseDoc ("Contract - " ++ cid) $ addNavBar CTxView cid $ renderCTVRs ctvrs'
  toMarkup (ContractViewError str) =
    baseDoc "An error occurred" (string ("Error: " ++ str))

data CIVR = CIVR { civrContractId :: String
                 , civrContractIdLink :: String
                 , civrBlockHeaderHash :: String
                 , civrBlockNo :: Integer
                 , civrBlockLink :: String
                 , civrSlotNo :: Integer
                 , civrRoleTokenMintingPolicyId :: String
                 , civrRoleTokenMintingPolicyIdLink :: String
                 , civrTags :: Map String String
                 , civrStatus :: String
                 , civrVersion :: String
                 }

renderCIVR :: CIVR -> Html
renderCIVR (CIVR { civrContractId = cid
                 , civrContractIdLink = cidLink
                 , civrBlockHeaderHash = blockHash
                 , civrBlockNo = blockNum
                 , civrBlockLink = blockLink
                 , civrSlotNo = slotNum
                 , civrRoleTokenMintingPolicyId = roleMintingPolicyId
                 , civrRoleTokenMintingPolicyIdLink = roleMintingPolicyIdLink
                 , civrTags = civrTags'
                 , civrStatus = contractStatus
                 , civrVersion = marloweVersion
                 }) =
  table $ do tr $ do td $ b "Contract ID"
                     td $ a ! href (toValue cidLink) $ string cid
             tr $ do td $ b "Block Header Hash"
                     td $ string blockHash
             tr $ do td $ b "Block No"
                     td $ a ! href (toValue blockLink) $ string $ show blockNum
             tr $ do td $ b "Slot No"
                     td $ string (show slotNum)
             tr $ do td $ b "Role Token Minting Policy ID"
                     td $ a ! href (toValue roleMintingPolicyIdLink) $ string roleMintingPolicyId
             tr $ do td $ b "Tags"
                     td $ renderTags civrTags'
             tr $ do td $ b "Status"
                     td $ string contractStatus
             tr $ do td $ b "Version"
                     td $ string marloweVersion

data CSVR = CSVR { csvrContractId :: String
                 , csvrContractIdLink :: String
                 , currentContract :: Maybe Contract
                 , initialContract :: Contract
                 , currentState :: Maybe State
                 , csvrBlockExplHost :: String
                 }

renderCSVR :: CSVR -> Html
renderCSVR (CSVR { csvrContractId = cid
                 , csvrContractIdLink = cidLink
                 , currentContract = cc
                 , initialContract = ic
                 , currentState = cs
                 , csvrBlockExplHost = blockExplHost
                 }) =
  table $ do tr $ do td $ b "Contract ID"
                     td $ a ! href (toValue cidLink) $ string cid
             tr $ do td $ b "Current contract"
                     td $ renderMContract cc
             tr $ do td $ b "Current state"
                     td $ renderMState blockExplHost cs
             tr $ do td $ b "Initial contract"
                     td $ renderMContract (Just ic)

data CTVRTDetail = CTVRTDetail
  {
    txPrev :: Maybe String,
    txNext :: Maybe String,
    txBlockHeaderHash :: String,
    txBlockNo :: Integer,
    txBlockLink :: String,
    txSlotNo :: Integer,
    inputs :: [Input],
    invalidBefore :: UTCTime,
    invalidHereafter :: UTCTime,
    outputContract :: Maybe Contract,
    outputState :: Maybe State,
    txStatus :: String,
    txTags :: Map String String,
    transactionId :: String
  }
  deriving Show

data CTVR = CTVR
  { ctvrBlock :: Integer
  , ctvrBlockLink :: String
  , ctvrSlot :: Integer
  , ctvrContractId :: String
  , ctvrTransactionId :: String
  }
  deriving Show

data CTVRs = CTVRs {
    ctvrsContractId :: String
  , ctvrs :: [CTVR]
  , ctvrsSelectedTransactionInfo :: Maybe CTVRTDetail
  , ctvrsBlockExplHost :: String
}


renderCTVRs :: CTVRs -> Html
renderCTVRs CTVRs { ctvrs = [] } = p ! style "color: red" $ string "There are no transactions"
renderCTVRs CTVRs { ctvrsContractId = ctvrsContractId'
                  , ctvrs = ctvrs'
                  , ctvrsSelectedTransactionInfo = ctvrsSelectedTransactionInfo'
                  , ctvrsBlockExplHost = blockExplHost
                  } = do
  table $ do
    tr $ do
      th $ b "Transaction ID"
      th $ b "Block No"
      th $ b "Slot No"
    forM_ ctvrs' makeRow
  renderCTVRTDetail ctvrsContractId' blockExplHost ctvrsSelectedTransactionInfo'
  where makeRow CTVR { ctvrBlock = ctvrBlock'
                     , ctvrSlot = ctvrSlot'
                     , ctvrContractId = ctvrContractId'
                     , ctvrTransactionId = ctvrTransactionId'
                     } = do
          tr $ do
            td $ if Just ctvrTransactionId' /= fmap transactionId ctvrsSelectedTransactionInfo'
                 then linkToTransaction ctvrContractId' ctvrTransactionId' ctvrTransactionId'
                 else string ctvrTransactionId'
            td $ string $ show ctvrBlock'
            td $ string $ show ctvrSlot'

renderCTVRTDetail :: String -> String -> Maybe CTVRTDetail -> Html
renderCTVRTDetail _cid _blockExplHost Nothing = do p $ string "Select a transaction to view its details"
renderCTVRTDetail cid blockExplHost (Just CTVRTDetail { txPrev = txPrev'
                                                      , txNext = txNext'
                                                      , txBlockHeaderHash = txBlockHeaderHash'
                                                      , txBlockNo = txBlockNo'
                                                      , txBlockLink = txBlockLink'
                                                      , txSlotNo = txSlotNo'
                                                      , inputs = inputs'
                                                      , invalidBefore = invalidBefore'
                                                      , invalidHereafter = invalidHereafter'
                                                      , outputContract = outputContract'
                                                      , outputState = outputState'
                                                      , txStatus = txStatus'
                                                      , txTags = tags'
                                                      , transactionId = transactionId'
                                                      }) =
  table $ do
  tr $ do
    td $ maybe (string previousTransactionLabel) (explorerTransactionLinkFromRuntimeLink previousTransactionLabel) txPrev'
    td $ maybe (string nextTransactionLabel) (explorerTransactionLinkFromRuntimeLink nextTransactionLabel) txNext'
  table $ do
    tr $ do
      td $ b "Block header hash"
      td $ string txBlockHeaderHash'
    tr $ do
      td $ b "Block number"
      td $ a ! href (toValue txBlockLink') $ string $ show txBlockNo'
    tr $ do
      td $ b "Slot number"
      td $ string $ show txSlotNo'
    tr $ do
      td $ b "Inputs"
      td $ do if null inputs' 
              then string "No inputs"
              else table $ do
                     mapM_ (\inp -> do tr $ td $ code $ stringToHtml $ show $ pretty inp) inputs'
    tr $ do
      td $ b "Invalid before"
      td $ makeLocalDateTime invalidBefore'
    tr $ do
      td $ b "Invalid after"
      td $ makeLocalDateTime invalidHereafter'
    tr $ do
      td $ b "Output Contract"
      td $ renderMContract outputContract'
    tr $ do
      td $ b "Output State"
      td $ renderMState blockExplHost outputState'
    tr $ do
      td $ b "Status"
      td $ string txStatus'
    tr $ do
      td $ b "Tags"
      td $ renderTags tags'
    tr $ do
      td $ b "Transaction Id"
      td $ a ! href (toValue $ "https://" ++ blockExplHost ++ "/transaction/" ++ transactionId') $ string transactionId'
  where previousTransactionLabel = "< Previous Transaction"
        nextTransactionLabel = "Next Transaction >"
        explorerTransactionLinkFromRuntimeLink label rtTxLink =
          case split '/' rtTxLink of
            [_, _, _, tTransactionId] -> linkToTransaction cid tTransactionId label
            _ -> string label

renderTags :: Map String String -> Html
renderTags tagMap | Map.null tagMap = string "No tags"
                  | otherwise = table $ do tr $ do
                                             th $ b "Tag"
                                             th $ b "Value"
                                           mapM_ (\(t, v) -> tr $ do
                                                               td $ string t
                                                               td $ string (show v)
                                                 ) (Map.toList tagMap)

renderParty :: String -> Party -> Html
renderParty blockExplHost (Address ad) = do string "Address: "
                                            a ! href (toValue ("https://" ++ blockExplHost ++ "/address/" ++ T.unpack ad))
                                              $ string $ unpack ad
renderParty _blockExplHost (Role ro) = string $ "Role: " ++ unpack ro

renderMAccounts :: String -> Map (Party, Token) Money -> Html
renderMAccounts blockExplHost mapAccounts = table $ do
  tr $ do
    th $ b "Party"
    th $ b "Currency (token name)"
    th $ b "Amount"
  let mkRow ((party, token), money) =
        let (tokenString, moneyString) = renderToken token money in
        tr $ do
          td $ renderParty blockExplHost party
          td $ string tokenString
          td $ string moneyString
  mapM_ mkRow $ Map.toList mapAccounts

renderToken :: Token -> Money -> (String, String)
renderToken (Token "" "") money = ("ADA", prettyPrintAmount 6 money)
renderToken (Token currSymbol tokenName) money = (printf "%s (%s)" currSymbol tokenName, prettyPrintAmount 0 money)

renderBoundValues :: Map ValueId Integer -> Html
renderBoundValues mapBoundValues = table $ do
  tr $ do
    th $ b "Value Id"
    th $ b "Value"
  let mkRow (ValueId valueId, bindingValue) =
        tr $ do
          td $ string $ T.unpack valueId
          td $ string $ prettyPrintAmount 0 bindingValue
  mapM_ mkRow $ Map.toList mapBoundValues

renderChoices :: String -> Map ChoiceId Integer -> Html
renderChoices blockExplHost mapChoices = table $ do
  tr $ do
    th $ b "Choice Id"
    th $ b "Party"
    th $ b "Value"
  let mkRow (ChoiceId choiceId party, choiceValue) =
        tr $ do
          td $ string $ T.unpack choiceId
          td $ renderParty blockExplHost party
          td $ string $ prettyPrintAmount 0 choiceValue
  mapM_ mkRow $ Map.toList mapChoices

renderTime :: POSIXTime -> Html
renderTime =
  makeLocalDateTime  -- ..and format it.
  . posixSecondsToUTCTime  -- ..convert to UTCTime for the formatting function..
  . realToFrac . (/ (1000 :: Double)) . fromIntegral  -- ..convert from millis to epoch seconds..
  . getPOSIXTime  -- Get the Integer out of our custom type..

renderMState :: String -> Maybe State -> Html
renderMState _blockExplHost Nothing = string "Contract closed"
renderMState blockExplHost (Just (State { accounts    = accs
                          , choices     = chos
                          , boundValues = boundVals
                          , minTime     = mtime })) =
  table $ do tr $ do td $ b "Accounts"
                     td $ ifEmptyMap accs (string "No accounts") $ renderMAccounts blockExplHost
             tr $ do td $ b "Bound values"
                     td $ ifEmptyMap boundVals (string "No bound values") renderBoundValues
             tr $ do td $ b "Choices"
                     td $ ifEmptyMap chos (string "No choices") $ renderChoices blockExplHost
             tr $ do td $ b "minTime"
                     td $ do renderTime mtime
                             string $ " (POSIX: " ++ show mtime ++ ")"

ifEmptyMap :: Map a b -> Html -> (Map a b -> Html) -> Html
ifEmptyMap mapToCheck defaultHtml renderMapFunc
  | M.null mapToCheck = defaultHtml
  | otherwise = renderMapFunc mapToCheck

renderMContract :: Maybe Contract -> Html
renderMContract Nothing = string "Contract closed"
renderMContract (Just c) = code $ stringToHtml $ show $ pretty c

addNavBar :: ContractViews -> String -> Html -> Html
addNavBar cv cid c = do
  table $ do tr $ do td $ b $ a ! href "listContracts" $ "Contracts List"
                     td $ b "Navigation bar"
                     mapM_ (\ccv -> mkNavLink (cv == ccv) cid (getNavTab ccv) (getNavTitle ccv))
                           allContractViews
                     td $ a ! href (toValue $ generateLink "contractDownloadInfo" [("contractId", cid)])
                            $ string "Download contract info"
  c

linkToTransaction :: String -> String -> String -> Html
linkToTransaction contractId transactionId' linkText =
  a ! href (toValue $ generateLink "contractView" [("tab", getNavTab CTxView), ("contractId", contractId), ("transactionId", transactionId')])
    $ string linkText

mkNavLink :: Bool -> String -> String -> String -> Html
mkNavLink True _ _ tabTitle =
  td $ string tabTitle
mkNavLink False cid tabName tabTitle =
  td $ a ! href (toValue $ generateLink "contractView" [("tab", tabName), ("contractId", cid)])
         $ string tabTitle

