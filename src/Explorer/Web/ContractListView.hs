{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Control.Newtype.Generics (op)
import Data.Time.Clock ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, preEscapedToHtml, string, toHtml, toValue )
import Text.Blaze.Html5.Attributes ( href, style )
import Text.Printf ( printf )

import Control.Concurrent.Var ( Var, readVar )
import Explorer.Web.Util ( baseDoc, generateLink, table, td, th, tr )
import qualified Language.Marlowe.Runtime.Types.Common as Common
import Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractInList(..)
  , ContractList(..)
  , Resource(..)
  )
import Opts (BlockExplorerHost(..), Options(optBlockExplorerHost))


data ContractListView
  = ContractListView
      UTCTime      -- Time of rendering (set to now when contractListView is called)
      UTCTime      -- Time of last contracts list retrieval from Marlowe Runtime
      (Maybe Int)  -- Page of contract ids to display
      [CLVR]       -- Contract list view records
  | ContractListViewError String

instance ToMarkup ContractListView where
  toMarkup :: ContractListView -> Markup
  toMarkup = renderCLVRs


data CLVR = CLVR
  { clvrContractId :: String
  , clvrBlock :: Integer
  , clvrSlot :: Integer
  , clvrRoleMintingPolicyId :: String
  , clvrLink :: String
  , clvrBlockExplLink :: String
  }

extractInfo :: UTCTime -> String -> Maybe Int -> ContractList -> ContractListView
extractInfo timeNow blockExplHost mbPage (ContractList retrievalTime cils) =
  ContractListView timeNow retrievalTime mbPage . map convertContract $ cils
  where
    convertContract :: ContractInList -> CLVR
    convertContract cil = CLVR
      { clvrContractId = cid
      , clvrBlock = Common.blockNo . resBlock . cilResource $ cil
      , clvrSlot = Common.slotNo . resBlock . cilResource $ cil
      , clvrRoleMintingPolicyId = resRoleTokenMintingPolicyId . cilResource $ cil
      , clvrLink = Common.linkUrl . cilLink $ cil
      , clvrBlockExplLink = printf "https://%s/transaction/%s" blockExplHost cid
      }
      where cid = resContractId . cilResource $ cil

contractListView :: Options -> Var ContractList -> Maybe Int -> IO ContractListView
contractListView opts varContractList mbPage = do
  let
    blockExplHost = op BlockExplorerHost . optBlockExplorerHost $ opts
  timeNow <- getCurrentTime
  extractInfo timeNow blockExplHost mbPage <$> readVar varContractList


renderTime :: UTCTime -> UTCTime -> Html
renderTime timeNow retrievalTime = do
  let
    -- Time formatters
    formatTime' = formatTime defaultTimeLocale "%F %T %Z"
    formatM = formatTime defaultTimeLocale "%M"
    formatS = formatTime defaultTimeLocale "%S"

    oneMinute :: NominalDiffTime
    oneMinute = 60

    difference = diffUTCTime timeNow retrievalTime

  if difference > oneMinute
    then do
      p ! style "color: red" $ string (printf "The list of contracts could not be updated for the last %s minute(s) and %s second(s), check the Marlowe Runtime is accessible" (formatM difference) (formatS difference))
    else p $ string ("Contracts list acquired: " <> formatTime' retrievalTime)


renderCLVRs :: ContractListView -> Html

renderCLVRs (ContractListView timeNow retrievalTime mbPage clvrs) = baseDoc "Marlowe Contract List" $ do
  let
    page = normalizePage (length clvrs) mbPage
    pageOfClvrs = takePage page clvrs
  renderTime timeNow retrievalTime
  table $ do
    tr $ do
      th $ b "Contract ID"
      th $ b "Role token minting policy"
      th $ b "Block No"
      th $ b "Slot No"
      th $ b ""
    let makeRow clvr = do
          let cid = clvrContractId clvr
          tr $ do
            td $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
              $ string cid
            td $ renderStr . clvrRoleMintingPolicyId $ clvr
            td $ toHtml . clvrBlock $ clvr
            td $ toHtml . clvrSlot $ clvr
            td $ a ! href (toValue $ clvrBlockExplLink clvr) $ string "Explore"
    forM_ pageOfClvrs makeRow
  renderNavBar page (length clvrs)

renderCLVRs (ContractListViewError msg) =
  baseDoc "An error occurred" $ string ("Error: " <> msg)


generateLink' :: Int -> String -> Html
generateLink' targetPage label =
  a ! href (toValue $ generateLink "listContracts" [("page", show targetPage)]) $ string label

space :: Html
space = preEscapedToHtml ("&nbsp;&nbsp;" :: String)

calcLastPage :: Int -> Int
calcLastPage numContracts = div numContracts pageLength + 1

renderNavBar :: Int -> Int -> Html
renderNavBar page numContracts = p $ do
  generateNavLink page 1 1 "<<"
  space
  generateNavLink page (page - 1) 1 "<"
  space
  renderNavNumbers page numContracts
  generateNavLink page (page + 1) lastPage ">"
  space
  generateNavLink page lastPage lastPage ">>"
  space
  string $ printf "| %d-%d contracts shown out of %d, (page %d out of %d)"
    ((page - 1) * pageLength + 1)
    (min (((page - 1) * pageLength) + pageLength) numContracts)
    numContracts
    page lastPage

  where
    lastPage = calcLastPage numContracts

    generateNavLink curPage targetPage comparisonPage
      | curPage == comparisonPage = string
      | otherwise = generateLink' targetPage


renderNavNumbers :: Int -> Int -> Html
renderNavNumbers page numContracts = mapM_ generateNumLink generatePageList
  where
    lastPage = calcLastPage numContracts

    generatePageList
      | page < 6 = [1..11]
      | page > lastPage - 5 = [lastPage - 10..lastPage]
      | otherwise = [page - 5..page + 5]

    generateNumLink curLink
      | page == curLink = (string . show $ curLink) >> space
      | otherwise = generateLink' curLink (show curLink) >> space


-- We're using this all over the place, setting it once here. Could be
-- parameterized in the future.
pageLength :: Int
pageLength = 20

-- Given the length of the data we have to show, make sure the page value we
-- received makes sense, or set it to 1
normalizePage :: Int -> Maybe Int -> Int
normalizePage 0            _           = 1  -- Sometimes the list hasn't loaded from the Runtime yet!
normalizePage numContracts (Just page) = if page <= calcLastPage numContracts then page else 1
normalizePage _            Nothing     = 1

-- Take the section of the given list corresponding to the page specified
takePage :: Int -> [a] -> [a]
takePage page = take pageLength . drop ((page - 1) * pageLength)


renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
