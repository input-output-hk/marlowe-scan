{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Control.Newtype.Generics (op)
import Data.Time.Clock ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, preEscapedToHtml, string, toHtml, toValue )
import Text.Blaze.Html5.Attributes ( href, style )
import Text.Printf ( printf )

import Explorer.SharedContractCache ( ContractListCache, readContractList )
import Explorer.Web.Util ( baseDoc, generateLink, table, td, th, tr, formatTimeDiff, makeLocalDateTime )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractInList (..), ContractLinks (..), Resource(..), ContractList (..), ContractInList (..) )
import Opts (BlockExplorerHost(..), Options(optBlockExplorerHost))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.List (intersperse)
import qualified Language.Marlowe.Runtime.Types.IndexedSeq as ISeq
import qualified Language.Marlowe.Runtime.Types.ContractsJSON as CSJ
import Explorer.API.IsContractOpen (isOpenAJAXBox)
import Explorer.API.GetNumTransactions (numTransactionsAJAXBox)

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

data ContractListView
  = ContractListView CLVR
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
  , clvrBlockExplLink :: String
  }
  deriving (Show, Eq)

-- Number of contracts per page
pageLength :: Int
pageLength = 20

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

extractInfo :: UTCTime -> String -> Maybe Int -> ContractList -> ContractListView
extractInfo _timeNow _blockExplHost _mbPage (ContractList { clRetrievedTime = Nothing }) = ContractListViewStillSyncing
extractInfo timeNow blockExplHost mbPage (ContractList { clRetrievedTime = Just retrievalTime
                                                       , clContracts = cils })
    | numContracts == 0 = ContractListViewError "There are no contracts in this network"
    | otherwise =
  ContractListView CLVR { timeOfRendering = timeNow
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
      , clvrBlockExplLink = printf "https://%s/transaction/%s" blockExplHost cilContractId
      }

contractListView :: Options -> ContractListCache -> Maybe Int -> IO ContractListView
contractListView opts contractListCache mbPage = do
  let
    blockExplHost = op BlockExplorerHost . optBlockExplorerHost $ opts
  timeNow <- getCurrentTime
  cl <- readContractList contractListCache
  return $ extractInfo timeNow blockExplHost mbPage cl


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
renderCIRs (ContractListView CLVR { timeOfRendering = timeNow
                                  , lastRetrieval = retrievalTime
                                  , pageInfo = pinf@(PageInfo { currentPage = page
                                                              , totalContracts = numContracts
                                                              , contractRange = (firstContract, lastContract)
                                                              , numPages = lastPage
                                                              })
                                  , contractList = clvrs
            }) = baseDoc "Marlowe Contract List" $ do
  renderTime timeNow retrievalTime
  p $ string $ printf "%d-%d contracts shown out of %d, (page %d out of %d)"
                        firstContract lastContract numContracts page lastPage
  table $ do
    tr $ do
      th $ b "Contract ID"
      th $ b "Role token minting policy"
      th $ b "Block No"
      th $ b "Slot No"
      th $ b "Status"
      th $ b "Num transactions"
      th $ b ""
    let makeRow clvr = do
          let cid = clvrContractId clvr
          tr $ do
            td $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
              $ string cid
            td $ renderStr $ clvrRoleMintingPolicyId clvr
            td $ toHtml $ clvrBlock clvr
            td $ toHtml $ clvrSlot clvr
            td $ isOpenAJAXBox cid
            td $ numTransactionsAJAXBox cid
            td $ a ! href (toValue $ clvrBlockExplLink clvr) $ string "Explore"
    forM_ clvrs makeRow
  renderNavBar pinf

renderCIRs ContractListViewStillSyncing =
  baseDoc "Marlowe Contract List" $ string "The explorer is still synchronising with the chain. Please, try again later"

renderCIRs (ContractListViewError msg) =
  baseDoc "An error occurred" $ string ("Error: " <> msg)


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
