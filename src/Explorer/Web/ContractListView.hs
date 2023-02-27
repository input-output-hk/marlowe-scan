{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Data.Time ( formatTime )
import Data.Time.Clock ( UTCTime )
import Data.Time.Format (defaultTimeLocale)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, p, string, toHtml, toValue )
import Text.Blaze.Html5.Attributes ( href )

import Control.Concurrent.Var ( Var, readVar )
import Explorer.Web.Util ( baseDoc, generateLink, table, td, th, tr )
import qualified Language.Marlowe.Runtime.Types.Common as Common
import Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractInList(..)
  , ContractList(..)
  , Resource(..)
  )


data ContractListView
  = ContractListView UTCTime [CLVR]
  | ContractListViewError String

instance ToMarkup ContractListView where
  toMarkup :: ContractListView -> Markup
  toMarkup (ContractListView retrievalTime clvrs) =
    baseDoc "Marlowe Contract List" $ renderCLVRs retrievalTime clvrs
  toMarkup (ContractListViewError msg) =
    baseDoc "An error occurred" $ string ("Error: " <> msg)

data CLVR = CLVR
  { clvrContractId :: String
  , clvrBlock :: Integer
  , clvrSlot :: Integer
  , clvrRoleMintingPolicyId :: String
  , clvrLink :: String
  }

extractInfo :: ContractList -> ContractListView
extractInfo (ContractList retrievalTime cils) = ContractListView retrievalTime . map convertContract $ cils
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
contractListView varContractList = extractInfo <$> readVar varContractList

renderCLVRs :: UTCTime -> [CLVR] -> Html
renderCLVRs retrievalTime clvrs = do
  p $ string ("Contracts list acquired: " <> formatTime defaultTimeLocale "%F %T %Z" retrievalTime)
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

renderStr :: String -> Html
renderStr "" = string "-"
renderStr s = string s
