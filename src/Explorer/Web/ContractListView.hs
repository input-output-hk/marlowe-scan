{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.ContractListView
  (ContractListView(..), contractListView)
  where

import Control.Monad (forM_)
import Text.Blaze.Html5 ( Html, Markup, ToMarkup(toMarkup), (!), a, b, string, toHtml, toValue )
import Text.Blaze.Html5.Attributes ( href, style )

import Explorer.Web.Util ( baseDoc, generateLink, table, td, th, tr )
import qualified Language.Marlowe.Runtime.Types.Common as Common
import Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractInList(..)
  , ContractList(..)
  , Resource(..)
  , getContracts
  )
import Opts (Options, mkUrlPrefix)


data ContractListView
  = ContractListView [CLVR]
  | ContractListViewError String

instance ToMarkup ContractListView where
  toMarkup :: ContractListView -> Markup
  toMarkup (ContractListView clvrs) =
    baseDoc "Marlowe Contract List (most recent 100 contracts)" $ renderCLVRs clvrs
  toMarkup (ContractListViewError msg) =
    baseDoc "An error occurred" $ string ("Error: " <> msg)

data CLVR = CLVR
  { clvrContractId :: String
  , clvrBlock :: Integer
  , clvrSlot :: Integer
  , clvrLink :: String
  }

extractInfo :: ContractList -> ContractListView
extractInfo (ContractList cils) = ContractListView . map convertContract $ cils
  where
    convertContract cil = CLVR
      { clvrContractId = resContractId . cilResource $ cil
      , clvrBlock = Common.blockNo . resBlock . cilResource $ cil
      , clvrSlot = Common.slotNo . resBlock . cilResource $ cil
      , clvrLink = Common.linkUrl . cilLink $ cil
      }

contractListView :: Options -> IO ContractListView
contractListView opts = do
  ecl <- getContracts (mkUrlPrefix opts)
  pure $ case ecl of
    Left str -> ContractListViewError str
    Right cl -> extractInfo cl

renderCLVRs :: [CLVR] -> Html
renderCLVRs clvrs = table ! style "border: 1px solid black" $ do
    tr $ do
      th $ b "Contract ID"
      th $ b "Block No"
      th $ b "Slot No"
    let makeRow clvr = do
          let cid = clvrContractId clvr
          tr $ do
            td $ a ! href (toValue $ generateLink "contractView" [("tab", "info"), ("contractId", cid)])
              $ string cid
            td $ toHtml . clvrBlock $ clvr
            td $ toHtml . clvrSlot $ clvr
    forM_ clvrs makeRow
