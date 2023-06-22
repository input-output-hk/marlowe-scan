{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Explorer.Web.ContractView
  (ContractView(..), contractView)
  where

import Control.Monad (forM_, forM, join)
import Control.Monad.Extra (whenMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Utils.Misc (split)
import Text.Blaze.Html5 (Html, Markup, ToMarkup(toMarkup), (!), a, b, code, p, string, ToValue (toValue), pre)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (href, style, class_)
import Text.Printf (printf)
import Explorer.Web.Util (tr, baseDoc, stringToHtml, prettyPrintAmount, makeLocalDateTime, generateLink,
                          mkTransactionExplorerLink, mkBlockExplorerLink, mkTokenPolicyExplorerLink,
                          valueToString, SyncStatus, downloadIcon, bookIcon, blockHeaderHashIcon,
                          roleTokenMintingPolicyIdIcon, slotNoIcon, blockNoIcon, metadataIcon, versionIcon,
                          statusIcon, dtd, inactiveLight, activeLight, mtd, dtable, makeTitleDiv, stateIcon,
                          createFullPopUp, alarmClockIcon, pptable, pptr, ppth, pptd, pptdWe, tableList, tlhr,
                          tlh, tlr, tldhc, tld, createPopUpLauncher, createPopUp, PopupLevel (..), circleIcon)
import Language.Marlowe.Pretty (pretty)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import qualified Language.Marlowe.Runtime.Types.TransactionsJSON as TJs
import qualified Language.Marlowe.Runtime.Types.TransactionJSON as TJ
import Language.Marlowe.Semantics.Types (ChoiceId(..), Contract, Input(..), Money, POSIXTime(..),
                                         Party(..), State(..), Token(..), ValueId(..), InputContent(..))
import Opts (Options (optBlockExplorerHost, Options), mkUrlPrefix, BlockExplorerHost (BlockExplorerHost))
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Prelude hiding (div)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Data.Map as M
import Explorer.SharedContractCache (ContractListCacheStatusReader(getSyncStatus))
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Explorer.Web.Pagination (bindVal, calcLastPage, calculateRange, PageInfo (..), PageLinkGenerator, renderNavBar)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as BS16

contractView :: ContractListCacheStatusReader contractCache => Options -> contractCache -> Maybe String -> Maybe String
                                                            -> Maybe Int -> Maybe String -> IO ContractView
contractView opts contractCache mTab mCid mPage mTxId = do
  curSyncStatus <- getSyncStatus contractCache
  cData <- contractViewInner opts mTab mCid mPage mTxId
  return $ ContractView { cvRetrievalSyncStatus = curSyncStatus
                        , cvMContractId = getMaybeContractId cData
                        , cvData = cData
                        }

getMaybeContractId :: CV -> Maybe String
getMaybeContractId (ContractInfoView CIVR { civrContractId = cid }) = Just cid
getMaybeContractId (ContractStateView CSVR { csvrContractId = cid }) = Just cid
getMaybeContractId (ContractTxListView CTLVRs { ctlvrsContractId = cid } ) = Just cid
getMaybeContractId (ContractTxView CTVR { ctvrContractId = cid } ) = Just cid
getMaybeContractId (ContractViewError _) = Nothing

contractViewInner :: Options -> Maybe String -> Maybe String -> Maybe Int -> Maybe String -> IO CV
contractViewInner opts@(Options {optBlockExplorerHost = BlockExplorerHost blockExplHost}) mTab (Just cid) mPage mTxId = do
  let urlPrefix = mkUrlPrefix opts
      tab = parseTab mTab
  r <- runExceptT (do cjson <- ExceptT $ CJ.getContractJSON urlPrefix cid
                      let link = CJ.transactions $ CJ.links cjson
                      txsjson <- whenMaybe (tab == CTxListView)
                                           $ ExceptT $ TJs.getContractTransactionsByLink urlPrefix link
                      txjson <- join <$> whenMaybe (tab == CTxView)
                                                   (forM mTxId (ExceptT . TJ.getTransaction urlPrefix link))
                      return $ extractInfo tab blockExplHost mPage cjson txsjson txjson)
  return $ either ContractViewError id r
contractViewInner opts Nothing cid mPage txId = contractViewInner opts (Just "state") cid mPage txId
contractViewInner _opts _tab Nothing _ _txId = return $ ContractViewError "Need to specify a contractId"

parseTab :: Maybe String -> ContractViews
parseTab (Just "state") = CStateView
parseTab (Just "tx-list") = CTxListView
parseTab (Just "tx") = CTxView
parseTab _ = CInfoView

-- Number of transactions per page
pageLength :: Int
pageLength = 12

-- Number of pages to each side to show (unless close to border)
contextPages :: Int
contextPages = 3

extractInfo :: ContractViews -> String -> Maybe Int -> CJ.ContractJSON -> Maybe TJs.Transactions -> Maybe TJ.Transaction -> CV
extractInfo CInfoView blockExplHost _ CJ.ContractJSON { CJ.resource =
                                           (CJ.Resource { CJ.block = CJ.Block { CJ.blockHeaderHash = blkHash
                                                                              , CJ.blockNo = blkNo
                                                                              , CJ.slotNo = sltNo
                                                                              }
                                                        , CJ.contractId = cid
                                                        , CJ.currentContract = currContract
                                                        , CJ.metadata =_metadata
                                                        , CJ.roleTokenMintingPolicyId = mintingPolicyId
                                                        , CJ.tags = tagsMap
                                                        , CJ.version = ver
                                                        })
                                                      } _ _ =
  ContractInfoView
      (CIVR { civrIsActive = isJust currContract
            , civrContractId = cid
            , civrContractIdLink = mkTransactionExplorerLink blockExplHost cid
            , civrBlockHeaderHash = blkHash
            , civrBlockNo = blkNo
            , civrBlockLink = mkBlockExplorerLink blockExplHost blkNo
            , civrSlotNo = sltNo
            , civrRoleTokenMintingPolicyId = mintingPolicyId
            , civrRoleTokenMintingPolicyIdLink = mkTokenPolicyExplorerLink blockExplHost mintingPolicyId
            , civrTags = fmap valueToString tagsMap
            , civrVersion = ver
            })
extractInfo CStateView blockExplHost _ CJ.ContractJSON { CJ.resource =
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
extractInfo CTxListView blockExplHost mbPage CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.contractId = cid }
                                                        }
            (Just (TJs.Transactions { TJs.transactions = txs })) mTx =
  ContractTxListView $ CTLVRs { ctlvrsContractId = cid
                              , ctlvrs = map convertTx transactions
                              , ctlvrsSelectedTransactionInfo = fmap (convertTxDetails blockExplHost) mTx
                              , ctlvrsBlockExplHost = blockExplHost
                              , ctlvrsPageInfo = PageInfo { currentPage = cPage
                                                          , pageRange = (minPage, maxPage)
                                                          , totalItems = numTransactions
                                                          , contractRange = (firstTransaction, lastTransaction)
                                                          , numPages = lastPage
                                                          }
                              }
  where
    firstTransaction = transactionsBefore + 1
    lastTransaction = transactionsBefore + length transactions
    transactionsBefore = pageLength * (cPage - 1)
    transactions = take pageLength $ drop transactionsBefore (reverse txs)
    numTransactions = length txs
    cPage = bindVal 1 lastPage $ fromMaybe 1 mbPage
    lastPage = calcLastPage pageLength numTransactions
    (minPage, maxPage) = calculateRange contextPages cPage lastPage
    convertTx TJs.Transaction { TJs.resource = TJs.Resource { TJs.block = TJs.Block { TJs.blockNo = blockNo'
                                                                                    , TJs.slotNo = slotNo'
                                                                                    }
                                                            , TJs.contractId = txContractId
                                                            , TJs.transactionId = transactionId'
                                                            }
                              } =
                   CTLVR { ctlvrBlock = blockNo'
                         , ctlvrBlockLink = mkBlockExplorerLink blockExplHost blockNo'
                         , ctlvrSlot = slotNo'
                         , ctlvrContractId = txContractId
                         , ctlvrTransactionId = transactionId'
                         }
extractInfo CTxView blockExplHost _ CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.contractId = cid }
                                                    }
            _ (Just tx) =
  ContractTxView $ CTVR { ctvrContractId = cid
                        , ctvrSelectedTransactionInfo = convertTxDetails blockExplHost tx
                        , ctvrBlockExplHost = blockExplHost
                        }
extractInfo CTxView _ _ _ _ Nothing = ContractViewError "Unable to find transaction to display"
extractInfo CTxListView _ _ _ Nothing _ = ContractViewError "Unable to fetch transaction list"


convertTxDetails :: String -> TJ.Transaction -> CTLVRTDetail
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
  = CTLVRTDetail { txPrev= mPrev
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
                , txTags = fmap valueToString txDetailTags
                , transactionId = txDetailTransactionId
                }


allContractViews :: [ContractViews]
allContractViews = [CInfoView, CStateView, CTxListView]

getNavTab :: ContractViews -> String
getNavTab CInfoView = "info"
getNavTab CStateView = "state"
getNavTab CTxListView = "tx-list"
getNavTab CTxView = "tx"

getNavTitle :: ContractViews -> String
getNavTitle CInfoView = "Details"
getNavTitle CStateView = "Code"
getNavTitle CTxListView = "Transactions"
getNavTitle CTxView = "Transaction Details"

data ContractViews = CInfoView
                   | CStateView
                   | CTxListView
                   | CTxView
  deriving (Show, Eq)

data ContractView = ContractView {
    cvRetrievalSyncStatus :: SyncStatus
  , cvMContractId :: Maybe String
  , cvData :: CV
}

instance ToMarkup ContractView where
  toMarkup :: ContractView -> Markup
  toMarkup (ContractView { cvRetrievalSyncStatus = curSyncStatus
                         , cvMContractId = Just cid
                         , cvData = cv
                         }) =
    baseDoc curSyncStatus ("Contract - " ++ cid) headerHtml $ toMarkup cv
    where
      headerHtml :: Html
      headerHtml = H.div ! class_ "contract-header"
                         $ do H.span ! class_ "contract-label"
                                     $ string "Contract - "
                              H.span ! class_ "contract-id"
                                     $ string cid

  toMarkup (ContractView { cvRetrievalSyncStatus = curSyncStatus
                         , cvMContractId = Nothing
                         , cvData = cv
                         }) =
    baseDoc curSyncStatus "Error" (makeTitleDiv "An error occurred") $ toMarkup cv

data CV = ContractInfoView CIVR
        | ContractStateView CSVR
        | ContractTxListView CTLVRs
        | ContractTxView CTVR
        | ContractViewError String

instance ToMarkup CV where
  toMarkup :: CV -> Markup
  toMarkup (ContractInfoView cvr@(CIVR {civrContractId = cid})) =
    addNavBar CInfoView cid $ renderCIVR cvr
  toMarkup (ContractStateView ccsr@(CSVR {csvrContractId = cid})) =
    addNavBar CStateView cid $ renderCSVR ccsr
  toMarkup (ContractTxListView ctlvrs'@CTLVRs {ctlvrsContractId = cid}) =
    addNavBar CTxListView cid $ renderCTLVRs ctlvrs'
  toMarkup (ContractTxView ctlvrs'@CTVR {ctvrContractId = cid}) =
    addNavBar CTxView cid $ renderCTVR ctlvrs'
  toMarkup (ContractViewError str) =
    string $ "Error: " ++ str


data CIVR = CIVR { civrIsActive :: Bool
                 , civrContractId :: String
                 , civrContractIdLink :: String
                 , civrBlockHeaderHash :: String
                 , civrBlockNo :: Integer
                 , civrBlockLink :: String
                 , civrSlotNo :: Integer
                 , civrRoleTokenMintingPolicyId :: String
                 , civrRoleTokenMintingPolicyIdLink :: String
                 , civrTags :: Map String String
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
                 , civrIsActive = contractStatus
                 , civrVersion = marloweVersion
                 }) =
  dtable $ do tr $ do dtd $ do statusIcon
                               string "Status"
                      mtd $ do H.span ! class_ "icon-margin-right"
                                      $ if contractStatus then activeLight else inactiveLight
                               H.span $ string $ if contractStatus then "Active" else "Closed"
              tr $ do dtd $ do bookIcon
                               string "Contract ID"
                      mtd $ a ! href (toValue cidLink) $ string cid
              tr $ do dtd $ do blockHeaderHashIcon
                               string "Block Header Hash"
                      mtd $ string blockHash
              tr $ do dtd $ do roleTokenMintingPolicyIdIcon
                               string "Role Token Minting Policy ID"
                      mtd $ a ! href (toValue roleMintingPolicyIdLink) $ string roleMintingPolicyId
              tr $ do dtd $ do slotNoIcon
                               string "Slot No"
                      mtd $ string (show slotNum)
              tr $ do dtd $ do blockNoIcon
                               string "Block No"
                      mtd $ a ! href (toValue blockLink) $ string $ show blockNum
              tr $ do dtd $ do metadataIcon
                               string "Tags"
                      mtd $ renderTags civrTags'
              tr $ do dtd $ do versionIcon
                               string "Version"
                      mtd $ string marloweVersion

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
                 , initialContract = ic
                 , currentState = cs
                 , csvrBlockExplHost = blockExplHost
                 }) = do
  dtable $ do tr $ do dtd $ do bookIcon
                               string "Contract ID"
                      mtd $ a ! href (toValue cidLink) $ string cid
              tr $ do dtd $ do stateIcon
                               string "Current state"
                      mtd $ renderMState blockExplHost cs
              renderMminTime cs
  renderMContract (Just ic)

renderMminTime :: Maybe State -> Html
renderMminTime cs = case cs of
                      Nothing -> return mempty
                      Just (State { minTime = mtime }) ->
                        tr $ do dtd $ do alarmClockIcon
                                         string "minTime"
                                mtd $ do renderTime mtime
                                         string $ " (POSIX: " ++ show mtime ++ ")"

data CTLVRTDetail = CTLVRTDetail
  { txPrev :: Maybe String
  , txNext :: Maybe String
  , txBlockHeaderHash :: String
  , txBlockNo :: Integer
  , txBlockLink :: String
  , txSlotNo :: Integer
  , inputs :: [Input]
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , outputContract :: Maybe Contract
  , outputState :: Maybe State
  , txStatus :: String
  , txTags :: Map String String
  , transactionId :: String
  }
  deriving Show

data CTLVR = CTLVR
  { ctlvrBlock :: Integer
  , ctlvrBlockLink :: String
  , ctlvrSlot :: Integer
  , ctlvrContractId :: String
  , ctlvrTransactionId :: String
  }
  deriving Show

data CTLVRs = CTLVRs
  { ctlvrsContractId :: String
  , ctlvrs :: [CTLVR]
  , ctlvrsSelectedTransactionInfo :: Maybe CTLVRTDetail
  , ctlvrsBlockExplHost :: String
  , ctlvrsPageInfo :: PageInfo
  }
  deriving Show

data CTVR = CTVR
  { ctvrContractId :: String
  , ctvrSelectedTransactionInfo :: CTLVRTDetail
  , ctvrBlockExplHost :: String
  }
  deriving Show

renderCTLVRs :: CTLVRs -> Html
renderCTLVRs CTLVRs { ctlvrs = [] } = p ! style "color: red" $ string "There are no transactions"
renderCTLVRs CTLVRs { ctlvrsContractId = cid
                    , ctlvrs = ctlvrs'
                    , ctlvrsSelectedTransactionInfo = ctlvrsSelectedTransactionInfo'
                    , ctlvrsPageInfo = pinf@(PageInfo { currentPage = page
                                                      , totalItems = numTransactions
                                                      , contractRange = (firstTransaction, lastTransaction)
                                                      , numPages = lastPage
                                                      })
                    } = do
  p $ string $ printf "%d-%d transactions shown out of %d, (page %d out of %d)"
                        firstTransaction lastTransaction numTransactions page lastPage
  tableList $ do
    tlhr $ do
      tlh $ b "Transaction ID"
      tlh $ b "Block No"
      tlh $ b "Slot No"
    forM_ ctlvrs' makeRow
  renderNavBar generateLink' pinf
  where makeRow CTLVR { ctlvrBlock = ctlvrBlock'
                      , ctlvrSlot = ctlvrSlot'
                      , ctlvrContractId = ctlvrContractId'
                      , ctlvrTransactionId = ctlvrTransactionId'
                      } = do
          tlr $ do
            tldhc $ if Just ctlvrTransactionId' /= fmap transactionId ctlvrsSelectedTransactionInfo'
                    then linkToTransaction ctlvrContractId' ctlvrTransactionId' ctlvrTransactionId'
                    else string ctlvrTransactionId'
            tld $ string $ show ctlvrBlock'
            tld $ string $ show ctlvrSlot'

        generateLink' :: PageLinkGenerator
        generateLink' targetPage = a ! class_ "invisible-link"
                                     ! href (toValue $ generateLink "contractView"
                                                                    [ ("tab", getNavTab CTxListView)
                                                                    , ("contractId", cid)
                                                                    , ("page", show targetPage)
                                                                    ])


renderCTLVRTDetail :: String -> String -> CTLVRTDetail -> Html
renderCTLVRTDetail cid blockExplHost (CTLVRTDetail { txPrev = txPrev'
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
                                                   , txTags = tags'
                                                   , transactionId = transactionId'
                                                   }) = do
  H.div ! class_ "pagination-box"
        $ do p $ string ""
             maybe (H.div $ previousTransactionLabel False) (explorerTransactionLinkFromRuntimeLink previousTransactionLabel) txPrev'
             maybe (H.div $ nextTransactionLabel False) (explorerTransactionLinkFromRuntimeLink nextTransactionLabel) txNext'
             p $ string ""
  dtable $ do
    tr $ do
      dtd $ do bookIcon
               string "Transaction Id"
      mtd $ a ! href (toValue $ "https://" ++ blockExplHost ++ "/transaction/" ++ transactionId') $ string transactionId'
    tr $ do
      dtd $ do alarmClockIcon
               string "Invalid before"
      mtd $ makeLocalDateTime invalidBefore'
    tr $ do
      dtd $ do alarmClockIcon
               string "Invalid after"
      mtd $ makeLocalDateTime invalidHereafter'
    tr $ do
      dtd $ do slotNoIcon
               string "Slot number"
      mtd $ string $ show txSlotNo'
    tr $ do
      dtd $ do blockHeaderHashIcon
               string "Block header hash"
      mtd $ string txBlockHeaderHash'
    tr $ do
      dtd $ do blockNoIcon
               string "Block number"
      mtd $ a ! href (toValue txBlockLink') $ string $ show txBlockNo'
    tr $ do
      dtd $ do circleIcon
               string "Inputs"
      mtd $ do if null inputs'
               then string "No inputs"
               else createInputsPopup blockExplHost inputs'
    tr $ do
      dtd $ do stateIcon
               string "Output State"
      mtd $ renderMState blockExplHost outputState'
    case outputState' of
      Nothing -> return mempty
      Just (State { minTime = mtime }) ->
        tr $ do dtd $ do alarmClockIcon
                         string "minTime"
                mtd $ do renderTime mtime
                         string $ " (POSIX: " ++ show mtime ++ ")"
    tr $ do
      dtd $ do metadataIcon
               string "Tags"
      mtd $ renderTags tags'
  renderMContract outputContract'
  where previousTransactionLabel enabled = makeTransactionLabel enabled "Previous Transaction"
        nextTransactionLabel enabled = makeTransactionLabel enabled "Next Transaction"
        makeTransactionLabel enabled = H.div ! class_ (if enabled then "page-button" else "page-button page-button-disabled")
        explorerTransactionLinkFromRuntimeLink linkContent rtTxLink =
          case split '/' rtTxLink of
            [_, _, _, tTransactionId] -> invisibleLinkToTransaction cid tTransactionId $ linkContent True
            _ -> H.div $ linkContent False

createInputsPopup :: String -> [Input] -> Html
createInputsPopup blockExplHost inputs' = do
  createFullPopUp "inputs" (show numInputs ++ " Input" ++ s)
                           inputsTable
  sequence_ popups
  where (inputsTable, popups) = renderInputs blockExplHost inputs'
        numInputs = length inputs'
        s | numInputs == 1 = ""
          | otherwise = "s"

renderInputs :: String -> [Input] -> (Html, [Html])
renderInputs blockExplHost inputs' = (pptable $ do
  pptr $ do
    ppth "Input type"
    ppth "Input owner"
    ppth "Details"
    ppth "Value"
    ppth "Continuation hash"
    ppth "Continuation contract"
  sequence_ rows, popups)
  where
    (rows, popups) = unzip $ [ renderInput blockExplHost idx inp
                             | (idx, inp) <- zip [1..] inputs' ]

renderInput :: String -> Integer -> Input -> (Html, Html)
renderInput blockExplHost _idx (NormalInput ic) =
  ( pptr $ do renderInputContent blockExplHost ic
              pptd $ string ""
              pptd $ string ""
  , mempty )
renderInput blockExplHost idx (MerkleizedInput ic chash cont) =
  ( pptr $ do renderInputContent blockExplHost ic
              pptdWe $ string $ BS.unpack $ BS16.encode chash
              pptd $ createPopUpLauncher popupId "Contract"
  , createPopUp PopupLevel2 popupId (renderMContract (Just cont)) )
 where popupId = "contract-" ++ show idx

renderInputContent :: String -> InputContent -> Html
renderInputContent blockExplHost (IDeposit party1 party2 token money) = do
  pptd "Deposit"
  pptdWe $ renderParty blockExplHost party2
  pptdWe $ do string "Account: "
              renderParty blockExplHost party1
  pptdWe $ string $ pamount ++ " " ++ tok
  where (tok, pamount) = renderToken token money
renderInputContent blockExplHost (IChoice (ChoiceId choiceName party) chosenNum) = do
  pptd "Choice"
  pptdWe $ renderParty blockExplHost party
  pptd $ do string "Choice Id: "
            string $ unpack choiceName
  pptd $ string $ show chosenNum
renderInputContent _ INotify = do
  pptd "Notify"
  pptd $ string ""
  pptd $ string ""
  pptd $ string ""

renderCTVR :: CTVR -> Html
renderCTVR CTVR { ctvrContractId = ctvrContractId'
                , ctvrSelectedTransactionInfo = ctlvrsSelectedTransactionInfo'
                , ctvrBlockExplHost = blockExplHost
                } = renderCTLVRTDetail ctvrContractId' blockExplHost ctlvrsSelectedTransactionInfo'

renderTags :: Map String String -> Html
renderTags tagMap | Map.null tagMap = string "No tags"
                  | otherwise = mapM_ (\(n, (t, v)) -> createFullPopUp ("tag_" ++ show n) t $
                                                         pre ! class_ "line-numbers" $ code ! class_ "language-javascript" $ stringToHtml v
                                      ) (zip [(1 :: Integer)..] $ Map.toList tagMap)

renderParty :: String -> Party -> Html
renderParty blockExplHost (Address ad) = do string "Address: "
                                            a ! href (toValue ("https://" ++ blockExplHost ++ "/address/" ++ T.unpack ad))
                                              $ string $ unpack ad
renderParty _blockExplHost (Role ro) = string $ "Role: " ++ unpack ro

renderMAccounts :: String -> Map (Party, Token) Money -> Html
renderMAccounts blockExplHost mapAccounts = pptable $ do
  pptr $ do
    ppth "Party"
    ppth "Currency (token name)"
    ppth "Amount"
  let mkRow ((party, token), money) =
        let (tokenString, moneyString) = renderToken token money in
        pptr $ do
          pptdWe $ renderParty blockExplHost party
          pptd $ string tokenString
          pptd $ string moneyString
  mapM_ mkRow $ Map.toList mapAccounts

renderToken :: Token -> Money -> (String, String)
renderToken (Token "" "") money = ("ADA", prettyPrintAmount 6 money)
renderToken (Token currSymbol tokenName) money = (printf "%s (%s)" currSymbol tokenName, prettyPrintAmount 0 money)

renderBoundValues :: Map ValueId Integer -> Html
renderBoundValues mapBoundValues = pptable $ do
  pptr $ do
    ppth "Value Id"
    ppth "Value"
  let mkRow (ValueId valueId, bindingValue) =
        pptr $ do
          pptd $ string $ T.unpack valueId
          pptd $ string $ prettyPrintAmount 0 bindingValue
  mapM_ mkRow $ Map.toList mapBoundValues

renderChoices :: String -> Map ChoiceId Integer -> Html
renderChoices blockExplHost mapChoices = pptable $ do
  pptr $ do
    ppth $ b "Choice Id"
    ppth $ b "Party"
    ppth $ b "Value"
  let mkRow (ChoiceId choiceId party, choiceValue) =
        pptr $ do
          pptd $ string $ T.unpack choiceId
          pptdWe $ renderParty blockExplHost party
          pptd $ string $ prettyPrintAmount 0 choiceValue
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
                                        , boundValues = boundVals })) =
  if null statePopUpList
  then string "No state"
  else mconcat statePopUpList
  where
  statePopUpList :: [Html]
  statePopUpList = catMaybes [ ifEmptyMap accs Nothing $ Just . createFullPopUp "accounts" "Accounts" . renderMAccounts blockExplHost
                             , ifEmptyMap boundVals Nothing $ Just . createFullPopUp "boundValues" "Bindings" . renderBoundValues
                             , ifEmptyMap chos Nothing $ Just . createFullPopUp "choices" "Choices" . renderChoices blockExplHost
                             ]

ifEmptyMap :: Map a b -> c -> (Map a b -> c) -> c
ifEmptyMap mapToCheck defaultHtml renderMapFunc
  | M.null mapToCheck = defaultHtml
  | otherwise = renderMapFunc mapToCheck

renderMContract :: Maybe Contract -> Html
renderMContract Nothing = H.div ! class_ "contract-code" $ string "Contract closed"
renderMContract (Just c) = pre ! class_ "line-numbers" $ code ! class_ "language-marlowe contract-code" $ stringToHtml $ show $ pretty c

addNavBar :: ContractViews -> String -> Html -> Html
addNavBar cv cid c = do
  H.div ! class_ "button-row"
        $ do mapM_ (\ccv -> mkNavLink (cv == ccv) cid (getNavTab ccv) (getNavTitle ccv))
                   allContractViews
             a ! class_ "invisible-link" ! href (toValue $ generateLink "contractDownloadInfo" [("contractId", cid)])
               $ H.div ! class_ "button-cell inactive-text"
                       $ do downloadIcon
                            string "Download"
  c

linkToTransaction :: String -> String -> String -> Html
linkToTransaction contractId transactionId' =
  customLinkToTransaction contractId transactionId' Nothing . string

invisibleLinkToTransaction :: String -> String -> Html -> Html
invisibleLinkToTransaction contractId transactionId' =
  customLinkToTransaction contractId transactionId' (Just "invisible-link")

customLinkToTransaction :: String -> String -> Maybe String -> Html -> Html
customLinkToTransaction contractId transactionId' =
  addMClass (a ! href (toValue $ generateLink "contractView" [("tab", getNavTab CTxView), ("contractId", contractId), ("transactionId", transactionId')]))
  where addMClass :: (Html -> Html) -> Maybe String -> Html -> Html
        addMClass f Nothing = f
        addMClass f (Just c) = f ! class_ (toValue c)

mkNavLink :: Bool -> String -> String -> String -> Html
mkNavLink isActive cid tabName tabTitle =
  a ! class_ "invisible-link" ! href (toValue $ generateLink "contractView" [("tab", tabName), ("contractId", cid)])
    $ H.div ! class_ (if isActive then "button-cell inactive-text active" else "button-cell inactive-text")
            $ string tabTitle

