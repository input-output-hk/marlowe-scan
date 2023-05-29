{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, string, toHtml, toValue )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes ( href, style, class_ )
import Text.Printf ( printf )

import Explorer.SharedContractCache ( ContractListCacheReader, readContractList, ContractList(..) )
import Explorer.Web.Util ( baseDoc, generateLink, formatTimeDiff, makeLocalDateTime, tableList, tlhr, tlh, tlr, tld, SyncStatus (..), tldhc )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractInList (..), ContractLinks (..), Resource(..), ContractInList (..), ContractListISeq )

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq
import qualified Language.Marlowe.Runtime.Types.ContractsJSON as CSJ
import Explorer.API.IsContractOpen (isOpenAJAXBox)
import Explorer.API.GetNumTransactions (numTransactionsAJAXBox)
import Opts (Options (..), TitleLabel (TitleLabel))
import Data.List.Extra (trim)

data PageInfo = PageInfo {
       currentPage :: Int
     , pageRange :: (Int, Int)
     , totalContracts :: Int
     , contractRange :: (Int, Int)
     , numPages :: Int
} deriving (Show, Eq)

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

-- Make sure a value is inside an interval
bindVal :: Int -> Int -> Int -> Int
bindVal minVal maxVal val = max (min val maxVal) minVal

-- Calculte the range of pages to display assuming:
-- * contextPages >= 0
-- * 1 >= page <= lastPage
-- * lastPage >= 1
calculateRange :: Int -> Int -> Int -> (Int, Int)
calculateRange ctxt page lastPage =
  if lastPage < 2 * ctxt + 1
  then (1, max 1 lastPage)
  else let centerPage = bindVal (ctxt + 1) (lastPage - ctxt) page
       in (centerPage - ctxt, centerPage + ctxt)

-- Calculates the number of pages available given the number of contracts and pageLength
calcLastPage :: Int -> Int
calcLastPage numContracts = fullPages + partialPages
  where fullPages = numContracts `div` pageLength
        sizeOfPartialPage = numContracts `rem` pageLength
        partialPages = if sizeOfPartialPage > 0 then 1 else 0

extractInfo :: SyncStatus -> Maybe Int -> ContractListISeq -> ContractListViewContents
extractInfo Syncing _mbPage _ = ContractListViewStillSyncing
extractInfo syncStatus mbPage cils
    | numContracts == 0 = ContractListViewError syncStatus "There are no contracts in this network"
    | otherwise =
  ContractListViewContents CLVR { pageInfo = PageInfo { currentPage = cPage
                                                      , pageRange = (minPage, maxPage)
                                                      , totalContracts = numContracts
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
    lastPage = calcLastPage numContracts
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
                                                                                 , totalContracts = numContracts
                                                                                 , contractRange = (firstContract, lastContract)
                                                                                 , numPages = lastPage
                                                                                 })
                                                     , contractList = clvrs
                                                     })
                             }) = baseDoc currSyncStatus ("Marlowe Contract List" `appIfNotBlank` labelForTitle) $ do
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
    renderNavBar pinf

renderCIRs (ContractListView { titleLabel = labelForTitle
                             , clvContents = ContractListViewStillSyncing}) =
  baseDoc Syncing ("Marlowe Contract List" `appIfNotBlank` labelForTitle) $ string "The explorer is still synchronising with the chain. Please, try again later"

renderCIRs (ContractListView { clvContents = ContractListViewError curSyncStatus msg }) =
  baseDoc curSyncStatus "An error occurred" $ string ("Error: " <> msg)

appIfNotBlank :: String -> String -> String
appIfNotBlank title labelForTitle
  | not $ null trimmedTitle = title ++ " (" ++ labelForTitle ++ ")"
  | otherwise = title
  where trimmedTitle = trim labelForTitle


generateLink' :: Int -> Html -> Html
generateLink' targetPage = a ! href (toValue $ generateLink "listContracts" [("page", show targetPage)])

renderNavBar :: PageInfo -> Html
renderNavBar pinf@(PageInfo { currentPage = page
                            , pageRange = (minPage, maxPage)
                            , numPages = lastPage
                            }) =
  H.div ! class_ "pagination-box"
        $ sequence_ $ [ generateNavLink False pinf 1 $ genArrow "<"
                      , generateNavLink True pinf (page - 1) $ genLink "Previous"
                      ] ++ [ generateNavLink False pinf np $ genLink $ show np | np <- [minPage..maxPage] ] ++
                      [ generateNavLink True pinf (page + 1) $ genLink "Next"
                      , generateNavLink False pinf lastPage $ genArrow ">"
                      ]


genArrow :: String -> Bool -> Html
genArrow label _ = H.div ! class_ "page-arrow" $ string label

genLink :: String -> Bool -> Html
genLink label isThis = H.div ! class_ (if isThis then "page-button current-page" else "page-button") $ string label

generateNavLink :: Bool -> PageInfo -> Int -> (Bool -> Html) -> Html
generateNavLink ommit (PageInfo { currentPage = page
                                , numPages = lastPage
                                }) targetPage genContent
  
  | page == boundPage && ommit = pure ()
  | otherwise = generateLink' boundPage
                  $ genContent (boundPage == page)
  where boundPage = bindVal 1 lastPage targetPage

renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
