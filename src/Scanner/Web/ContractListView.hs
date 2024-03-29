{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Scanner.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, string, toHtml, toValue )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes ( href, style, class_ )
import Text.Printf ( printf )

import Scanner.SharedContractCache ( ContractListCacheReader, readContractList, ContractList(..) )
import Scanner.Web.Util ( baseDoc, generateLink, formatTimeDiff, makeLocalDateTime, tableList, tlhr, tlh, tlr, tld, SyncStatus (..), tldhc, makeTitleDiv )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractInList (..), ContractLinks (..), Resource(..), ContractInList (..), ContractListISeq )

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq
import qualified Language.Marlowe.Runtime.Types.ContractsJSON as CSJ
import Scanner.API.IsContractOpen (isOpenAJAXBox)
import Scanner.API.GetNumTransactions (numTransactionsAJAXBox)
import Opts (Options (..), TitleLabel (TitleLabel))
import Data.List.Extra (trim)
import Scanner.Web.Pagination (PageInfo (..), renderNavBar, bindVal, calculateRange, PageLinkGenerator, calcLastPage)

data CLVR = CLVR {
      -- | Info about current page
       pageInfo :: PageInfo
      -- | Contract list view records
     , contractList :: [CIR]
  } deriving (Show, Eq)

data ContractListView = ContractListView { titleLabel :: String
                                         , retrievalSyncStatus :: SyncStatus
                                         , clvContents :: ContractListViewContents
                                         }

data ContractListViewContents
  = ContractListViewContents CLVR
  | ContractListViewStillSyncing
  | ContractListViewError SyncStatus String
  deriving (Show, Eq)

instance ToMarkup ContractListView where
  toMarkup :: ContractListView -> Markup
  toMarkup = renderCIRs

data CIR = CIR
  { clvrContractId :: String
  , clvrBlock :: Integer
  , clvrSlot :: Integer
  , clvrRoleMintingPolicyId :: String
  , clvrLink :: String
  }
  deriving (Show, Eq)

-- Number of contracts per page
pageLength :: Int
pageLength = 12

-- Number of pages to each side to show (unless close to border)
contextPages :: Int
contextPages = 3

extractInfo :: SyncStatus -> Maybe Int -> ContractListISeq -> ContractListViewContents
extractInfo Syncing _mbPage _ = ContractListViewStillSyncing
extractInfo syncStatus mbPage cils
    | numContracts == 0 = ContractListViewError syncStatus "There are no contracts in this network"
    | otherwise =
  ContractListViewContents CLVR { pageInfo = PageInfo { currentPage = cPage
                                                      , pageRange = (minPage, maxPage)
                                                      , totalItems = numContracts
                                                      , contractRange = (firstContract, lastContract)
                                                      , numPages = lastPage
                                                      }
                                , contractList = map convertContract $ toList contracts }
  where
    firstContract = contractsBefore + 1
    lastContract = contractsBefore + Seq.length contracts
    contractsBefore = pageLength * (cPage - 1)
    contracts = Seq.take pageLength $ ISeq.toSeq $ ISeq.drop contractsBefore cils
    numContracts = ISeq.length cils
    cPage = bindVal 1 lastPage $ fromMaybe 1 mbPage
    lastPage = calcLastPage pageLength numContracts
    (minPage, maxPage) = calculateRange contextPages cPage lastPage

    convertContract :: ContractInList -> CIR
    convertContract ContractInList { links = ContractLinks { contract = cilLinkUrl }
                                   , resource = Resource { block = CSJ.Block { CSJ.blockNo = cilBlockNo
                                                                             , CSJ.slotNo = cilSlotNo
                                                                             }
                                                         , contractId = cilContractId
                                                         , roleTokenMintingPolicyId = cilRoleTokenMintingPolicyId
                                                         }
                                   } = CIR
      { clvrContractId = cilContractId
      , clvrBlock = cilBlockNo
      , clvrSlot = cilSlotNo
      , clvrRoleMintingPolicyId = cilRoleTokenMintingPolicyId
      , clvrLink = cilLinkUrl
      }

contractListView :: ContractListCacheReader contractListCache => Options -> contractListCache -> Maybe Int -> IO ContractListView
contractListView Options { optTitleLabel = TitleLabel labelForTitle } contractListCache mbPage = do
  ( ContractList { clContracts = cl
                 , clSyncStatus = curSyncStatus }) <- readContractList contractListCache
  return $ ContractListView { titleLabel = labelForTitle
                            , clvContents = extractInfo curSyncStatus mbPage cl
                            , retrievalSyncStatus = curSyncStatus
                            }

renderTime :: SyncStatus -> Html
renderTime Syncing = p $ string "Retrieving list of contracts from Marlowe Runtime..."
renderTime (Synced _ lrt) = p $ do string "Contracts list acquired: "
                                   makeLocalDateTime lrt
renderTime (OutOfSync ndt _) = p ! style "color: red"
                                  $ string (printf "The list of contracts could not be updated since " ++ formatTimeDiff ndt ++ ", check the Marlowe Runtime is accessible")


renderCIRs :: ContractListView -> Html
renderCIRs (ContractListView { titleLabel = labelForTitle
                             , retrievalSyncStatus = currSyncStatus
                             , clvContents = ContractListViewContents
                                               (CLVR { pageInfo = pinf@(PageInfo { currentPage = page
                                                                                 , totalItems = numContracts
                                                                                 , contractRange = (firstContract, lastContract)
                                                                                 , numPages = lastPage
                                                                                 })
                                                     , contractList = clvrs
                                                     })
                             }) = baseDoc currSyncStatus headerText (makeTitleDiv headerText) $ do
  renderTime currSyncStatus
  p $ string $ printf "%d-%d contracts shown out of %d, (page %d out of %d)"
                        firstContract lastContract numContracts page lastPage
  H.div ! class_ "table-wrapper" $ do
    tableList $ do
      tlhr $ do
        tlh $ b "Active"
        tlh $ b "Contract ID"
        tlh $ b "Role token minting policy"
        tlh $ b "Block No"
        tlh $ b "Slot No"
        tlh $ b "Num transactions"
      let makeRow clvr = do
            let cid = clvrContractId clvr
            tlr $ do
              tld $ isOpenAJAXBox cid
              tldhc $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
                        $ string cid
              tldhc $ renderStr $ clvrRoleMintingPolicyId clvr
              tld $ toHtml $ clvrBlock clvr
              tld $ toHtml $ clvrSlot clvr
              tld $ numTransactionsAJAXBox cid
      forM_ clvrs makeRow
    renderNavBar generateLink' pinf
    where headerText = "Marlowe Contract List" `appIfNotBlank` labelForTitle
          
          generateLink' :: PageLinkGenerator
          generateLink' targetPage = a ! href (toValue $ generateLink "listContracts" [("page", show targetPage)])

renderCIRs (ContractListView { titleLabel = labelForTitle
                             , clvContents = ContractListViewStillSyncing}) =
  baseDoc Syncing headerText (makeTitleDiv headerText) $ string "MarloweScan is still synchronising with the chain. Please, try again later"
    where headerText = "Marlowe Contract List" `appIfNotBlank` labelForTitle

renderCIRs (ContractListView { clvContents = ContractListViewError curSyncStatus msg }) =
  baseDoc curSyncStatus "Error" (makeTitleDiv "An error occurred") $ string ("Error: " <> msg)


appIfNotBlank :: String -> String -> String
appIfNotBlank title labelForTitle
  | not $ null trimmedTitle = title ++ " (" ++ labelForTitle ++ ")"
  | otherwise = title
  where trimmedTitle = trim labelForTitle


renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
