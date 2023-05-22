{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Data.Time.Clock ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, preEscapedToHtml, string, toHtml, toValue )
import Text.Blaze.Html5.Attributes ( href, style )
import Text.Printf ( printf )

import Explorer.SharedContractCache ( ContractListCache, readContractList )
import Explorer.Web.Util ( baseDoc, generateLink, formatTimeDiff, makeLocalDateTime, tableList, tlhr, tlh, tlr, tld )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractInList (..), ContractLinks (..), Resource(..), ContractList (..), ContractInList (..) )

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.List (intersperse)
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
      -- | Time of rendering (set to now when contractListView is called)
       timeOfRendering :: UTCTime
      -- | Time of last contracts list retrieval from Marlowe Runtime
     , lastRetrieval :: UTCTime
      -- | Info about current page
     , pageInfo :: PageInfo
      -- | Contract list view records
     , contractList :: [CIR]
  } deriving (Show, Eq)

data ContractListView = ContractListView { titleLabel :: String
                                         , clvContents :: ContractListViewContents
                                         }

data ContractListViewContents
  = ContractListViewContents CLVR
  | ContractListViewStillSyncing
  | ContractListViewError String
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
contextPages = 5

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

extractInfo :: UTCTime -> Maybe Int -> ContractList -> ContractListViewContents
extractInfo _timeNow _mbPage (ContractList { clRetrievedTime = Nothing }) = ContractListViewStillSyncing
extractInfo timeNow mbPage (ContractList { clRetrievedTime = Just retrievalTime
                                         , clContracts = cils })
    | numContracts == 0 = ContractListViewError "There are no contracts in this network"
    | otherwise =
  ContractListViewContents CLVR { timeOfRendering = timeNow
                        , lastRetrieval = retrievalTime
                        , pageInfo = PageInfo { currentPage = cPage
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

contractListView :: Options -> ContractListCache -> Maybe Int -> IO ContractListView
contractListView Options { optTitleLabel = TitleLabel labelForTitle } contractListCache mbPage = do
  timeNow <- getCurrentTime
  cl <- readContractList contractListCache
  return $ ContractListView { titleLabel = labelForTitle
                            , clvContents = extractInfo timeNow mbPage cl }

renderTime :: UTCTime -> UTCTime -> Html
renderTime timeNow retrievalTime =
  if difference > delayBeforeWarning
    then do
      p ! style "color: red" $ string (printf "The list of contracts could not be updated since " ++ formatTimeDiff difference ++ ", check the Marlowe Runtime is accessible")
    else p $ do string "Contracts list acquired: "
                makeLocalDateTime retrievalTime
  where
    delayBeforeWarning :: NominalDiffTime
    delayBeforeWarning = 60  -- This is one minute
    
    difference = diffUTCTime timeNow retrievalTime

renderCIRs :: ContractListView -> Html
renderCIRs (ContractListView { titleLabel = labelForTitle
                             , clvContents = ContractListViewContents
                                               (CLVR { timeOfRendering = timeNow
                                                    , lastRetrieval = retrievalTime
                                                    , pageInfo = pinf@(PageInfo { currentPage = page
                                                                                , totalContracts = numContracts
                                                                                , contractRange = (firstContract, lastContract)
                                                                                , numPages = lastPage
                                                                                })
                                                    , contractList = clvrs
                                                    })
                             }) = baseDoc ("Marlowe Contract List" `appIfNotBlank` labelForTitle) $ do
  renderTime timeNow retrievalTime
  p $ string $ printf "%d-%d contracts shown out of %d, (page %d out of %d)"
                        firstContract lastContract numContracts page lastPage
  tableList $ do
    tlhr $ do
      tlh $ b "Contract ID"
      tlh $ b "Role token minting policy"
      tlh $ b "Block No"
      tlh $ b "Slot No"
      tlh $ b "Status"
      tlh $ b "Num transactions"
    let makeRow clvr = do
          let cid = clvrContractId clvr
          tlr $ do
            tld $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
                    $ string cid
            tld $ renderStr $ clvrRoleMintingPolicyId clvr
            tld $ toHtml $ clvrBlock clvr
            tld $ toHtml $ clvrSlot clvr
            tld $ isOpenAJAXBox cid
            tld $ numTransactionsAJAXBox cid
    forM_ clvrs makeRow
  renderNavBar pinf

renderCIRs (ContractListView { titleLabel = labelForTitle
                             , clvContents = ContractListViewStillSyncing}) =
  baseDoc ("Marlowe Contract List" `appIfNotBlank` labelForTitle) $ string "The explorer is still synchronising with the chain. Please, try again later"

renderCIRs (ContractListView { clvContents = ContractListViewError msg }) =
  baseDoc "An error occurred" $ string ("Error: " <> msg)

appIfNotBlank :: String -> String -> String
appIfNotBlank title labelForTitle
  | not $ null trimmedTitle = title ++ " (" ++ labelForTitle ++ ")"
  | otherwise = title
  where trimmedTitle = trim labelForTitle


generateLink' :: Int -> String -> Html
generateLink' targetPage label =
  a ! href (toValue $ generateLink "listContracts" [("page", show targetPage)]) $ string label

space :: Html
space = preEscapedToHtml ("&nbsp;&nbsp;" :: String)

renderNavBar :: PageInfo -> Html
renderNavBar pinf@(PageInfo { currentPage = page
                            , pageRange = (minPage, maxPage)
                            , numPages = lastPage
                            }) =
  p $ sequence_ $ intersperse space $ [ generateNavLink pinf 1 "<<"
                                      , generateNavLink pinf (page - 1) "<"
                                      ] ++ [ generateNavLink pinf np (show np) | np <- [minPage..maxPage] ] ++
                                      [ generateNavLink pinf (page + 1) ">"
                                      , generateNavLink pinf lastPage ">>"
                                      ]

generateNavLink :: PageInfo -> Int -> String -> Html
generateNavLink (PageInfo { currentPage = page
                          , numPages = lastPage
                          }) targetPage linkText
  | page == boundPage = string linkText
  | otherwise = generateLink' boundPage linkText
  where boundPage = bindVal 1 lastPage targetPage

renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
