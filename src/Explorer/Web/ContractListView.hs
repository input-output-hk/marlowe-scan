{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Data.Time.Clock ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, string, toHtml, toValue )
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


data ContractListView
  = ContractListView
      UTCTime  -- Time of rendering (set to now when contractListView is called)
      UTCTime  -- Time of last contracts list retrieval from Marlowe Runtime
      [CLVR]   -- Contract list view records
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
  }

extractInfo :: UTCTime -> ContractList -> ContractListView
extractInfo timeNow (ContractList retrievalTime cils) =
  ContractListView timeNow retrievalTime . map convertContract $ cils
  where
    convertContract :: ContractInList -> CLVR
    convertContract cil = CLVR
      { clvrContractId = resContractId . cilResource $ cil
      , clvrBlock = Common.blockNo . resBlock . cilResource $ cil
      , clvrSlot = Common.slotNo . resBlock . cilResource $ cil
      , clvrRoleMintingPolicyId = resRoleTokenMintingPolicyId . cilResource $ cil
      , clvrLink = Common.linkUrl . cilLink $ cil
      }

contractListView :: Var ContractList -> IO ContractListView
contractListView varContractList = do
  timeNow <- getCurrentTime
  extractInfo timeNow <$> readVar varContractList


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

renderCLVRs (ContractListView timeNow retrievalTime clvrs) = baseDoc "Marlowe Contract List" $ do
  renderTime timeNow retrievalTime
  table $ do
    tr $ do
      th $ b "Contract ID"
      th $ b "Role token minting policy"
      th $ b "Block No"
      th $ b "Slot No"
    let makeRow clvr = do
          let cid = clvrContractId clvr
          tr $ do
            td $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
              $ string cid
            td $ renderStr . clvrRoleMintingPolicyId $ clvr
            td $ toHtml . clvrBlock $ clvr
            td $ toHtml . clvrSlot $ clvr
    forM_ clvrs makeRow

renderCLVRs (ContractListViewError msg) =
  baseDoc "An error occurred" $ string ("Error: " <> msg)


renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
