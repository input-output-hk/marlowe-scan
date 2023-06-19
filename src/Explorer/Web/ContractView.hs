{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

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
import Text.Blaze.Html5 (Html, Markup, ToMarkup(toMarkup), (!), a, b, code, p, string, ToValue (toValue), pre)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (href, style, class_)
import Text.Printf (printf)
import Explorer.Web.Util (tr, th, td, table, baseDoc, stringToHtml, prettyPrintAmount, makeLocalDateTime,
                          generateLink, mkTransactionExplorerLink, mkBlockExplorerLink, mkTokenPolicyExplorerLink,
                          valueToString, SyncStatus, downloadIcon, contractIdIcon, blockHeaderHashIcon,
                          roleTokenMintingPolicyIdIcon, slotNoIcon, blockNoIcon, metadataIcon, versionIcon,
                          statusIcon, dtd, inactiveLight, activeLight, mtd, dtable, makeTitleDiv, stateIcon, createPopUpLauncher, alarmClockIcon, pptable, pptr, ppth, pptd, pptdWe)
import Language.Marlowe.Pretty (pretty)
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
import Explorer.SharedContractCache (ContractListCacheStatusReader(getSyncStatus))
import Data.Maybe (isJust, catMaybes)

contractView :: ContractListCacheStatusReader contractCache => Options -> contractCache -> Maybe String -> Maybe String -> Maybe String -> IO ContractView
contractView opts contractCache mTab mCid mTxId = do
  curSyncStatus <- getSyncStatus contractCache
  cData <- contractViewInner opts mTab mCid mTxId
  return $ ContractView { cvRetrievalSyncStatus = curSyncStatus
                        , cvMContractId = getMaybeContractId cData
                        , cvData = cData
                        }

getMaybeContractId :: CV -> Maybe String
getMaybeContractId (ContractInfoView CIVR { civrContractId = cid }) = Just cid
getMaybeContractId (ContractStateView CSVR { csvrContractId = cid }) = Just cid
getMaybeContractId (ContractTxView CTVRs { ctvrsContractId = cid } ) = Just cid
getMaybeContractId (ContractViewError _) = Nothing

contractViewInner :: Options -> Maybe String -> Maybe String -> Maybe String -> IO CV
contractViewInner opts@(Options {optBlockExplorerHost = BlockExplorerHost blockExplHost}) mTab (Just cid) mTxId = do
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
contractViewInner opts Nothing cid txId = contractViewInner opts (Just "state") cid txId
contractViewInner _opts _tab Nothing _txId = return $ ContractViewError "Need to specify a contractId"


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


extractInfo :: ContractViews -> String -> CJ.ContractJSON -> Maybe TJs.Transactions -> Maybe TJ.Transaction -> CV
extractInfo CInfoView blockExplHost CJ.ContractJSON { CJ.resource =
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
                , txTags = fmap valueToString txDetailTags
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
        | ContractTxView CTVRs
        | ContractViewError String

instance ToMarkup CV where
  toMarkup :: CV -> Markup
  toMarkup (ContractInfoView cvr@(CIVR {civrContractId = cid})) =
    addNavBar CInfoView cid $ renderCIVR cvr
  toMarkup (ContractStateView ccsr@(CSVR {csvrContractId = cid})) =
    addNavBar CStateView cid $ renderCSVR ccsr
  toMarkup (ContractTxView ctvrs'@CTVRs {ctvrsContractId = cid}) =
    addNavBar CTxView cid $ renderCTVRs ctvrs'
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
              tr $ do dtd $ do contractIdIcon
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
  dtable $ do tr $ do dtd $ do contractIdIcon
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
                                                      }) = do
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
      td $ b "Output State"
      td $ renderMState blockExplHost outputState'
    case outputState' of
      Nothing -> return mempty
      Just (State { minTime = mtime }) ->
        tr $ do td $ b "minTime"
                td $ do renderTime mtime
                        string $ " (POSIX: " ++ show mtime ++ ")"
    tr $ do
      td $ b "Status"
      td $ string txStatus'
    tr $ do
      td $ b "Tags"
      td $ renderTags tags'
    tr $ do
      td $ b "Transaction Id"
      td $ a ! href (toValue $ "https://" ++ blockExplHost ++ "/transaction/" ++ transactionId') $ string transactionId'
  renderMContract outputContract'
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
                                                               td $ code $ stringToHtml v
                                                 ) (Map.toList tagMap)

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
  statePopUpList = catMaybes [ ifEmptyMap accs Nothing $ Just . createPopUpLauncher "accounts" "Accounts" . renderMAccounts blockExplHost
                             , ifEmptyMap boundVals Nothing $ Just . createPopUpLauncher "boundValues" "Bindings" . renderBoundValues
                             , ifEmptyMap chos Nothing $ Just . createPopUpLauncher "choices" "Choices" . renderChoices blockExplHost
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
linkToTransaction contractId transactionId' linkText =
  a ! href (toValue $ generateLink "contractView" [("tab", getNavTab CTxView), ("contractId", contractId), ("transactionId", transactionId')])
    $ string linkText

mkNavLink :: Bool -> String -> String -> String -> Html
mkNavLink isActive cid tabName tabTitle =
  a ! class_ "invisible-link" ! href (toValue $ generateLink "contractView" [("tab", tabName), ("contractId", cid)])
    $ H.div ! class_ (if isActive then "button-cell inactive-text active" else "button-cell inactive-text")
            $ string tabTitle

