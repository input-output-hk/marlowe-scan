{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Util (SyncStatus(..), baseDoc, formatTimeDiff, generateLink, linkFor, makeLocalDateTime,
                          prettyPrintAmount, stringToHtml, table, td, th, tr, mkTransactionExplorerLink,
                          mkBlockExplorerLink, mkTokenPolicyExplorerLink, valueToString, tableList, tlh, tlhr,
                          tlr, tld, calculateSyncStatus, tldhc, downloadIcon, blockHeaderHashIcon, blockNoIcon,
                          contractIdIcon, metadataIcon, roleTokenMintingPolicyIdIcon, slotNoIcon, statusIcon,
                          versionIcon, dtd, activeLight, inactiveLight, mtd, dtable, makeTitleDiv, stateIcon, arrowDropDownIcon, createPopUpLauncher, baseJSScripts, alarmClockIcon)
  where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 ( pack, unpack )
import Network.HTTP.Types ( renderSimpleQuery )
import Prelude hiding ( head )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( body, docTypeHtml, head, html, title,
                          string, Html, (!), preEscapedString, a, ToValue (toValue), Markup, script, link, customAttribute, img, stringValue )
import Text.Blaze.Html5.Attributes ( style, lang, href, type_, rel, class_, src, alt, onclick )
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time (UTCTime, NominalDiffTime)
import Text.Printf (printf)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock (diffUTCTime)

crossorigin :: H.Attribute
crossorigin = customAttribute "crossorigin" ""

logo :: Html
logo = img ! src "/svg/logo.svg" ! alt "Marlowe logo" ! class_ "logo-pic"

magnifyingGlassIcon :: Html
magnifyingGlassIcon = img ! src "/svg/magnifying-glass.svg" ! alt "Magnifying glass" ! class_ "side-icon"

greenStatusLight :: Html
greenStatusLight = img ! src "/svg/green-status-light.svg" ! alt "Green status light"

amberStatusLight :: Html
amberStatusLight = img ! src "/svg/amber-status-light.svg" ! alt "Amber status light"

redStatusLight :: Html
redStatusLight = img ! src "/svg/red-status-light.svg" ! alt "Red status light"

alarmClockIcon :: Html
alarmClockIcon = img ! src "/svg/alarm_clock.svg" ! alt "Alarm clock" ! class_ "side-icon"

activeLight :: Html
activeLight = img ! src "/svg/active-light.svg" ! alt "Amber status light"

inactiveLight :: Html
inactiveLight = img ! src "/svg/inactive-light.svg" ! alt "Red status light"

fullLogo :: Html
fullLogo = a ! class_ "invisible-link" ! href "/"
             $ H.div ! class_ "logo"
                     $ do logo
                          H.div ! class_ "logo-text"
                                $ do H.span ! class_ "logo-text-marlowe" $ string "Marlowe"
                                     space
                                     H.span ! class_ "logo-text-explorer" $ string "Explorer"

downloadIcon :: Html
downloadIcon = img ! class_ "icon" ! src "/svg/download.svg" ! alt "Download icon"

arrowDropDownIcon :: Html
arrowDropDownIcon = img ! class_ "icon" ! src "/svg/arrow_drop_down.svg" ! alt "Arrow drop down icon"

blockHeaderHashIcon :: Html
blockHeaderHashIcon = img ! class_ "icon" ! src "/svg/block_header_hash.svg" ! alt "Block Header Hash icon"

blockNoIcon :: Html
blockNoIcon = img ! class_ "icon" ! src "/svg/block_no.svg" ! alt "Block Number icon"

contractIdIcon :: Html
contractIdIcon = img ! class_ "icon" ! src "/svg/contract_id.svg" ! alt "Contract ID icon"

metadataIcon :: Html
metadataIcon = img ! class_ "icon" ! src "/svg/metadata.svg" ! alt "Metadata icon"

roleTokenMintingPolicyIdIcon :: Html
roleTokenMintingPolicyIdIcon = img ! class_ "icon" ! src "/svg/role_token_minting_policy_id.svg" ! alt "Role Token Minting Policy ID icon"

slotNoIcon :: Html
slotNoIcon = img ! class_ "icon" ! src "/svg/slot_no.svg" ! alt "Slot Number icon"

stateIcon :: Html
stateIcon = img ! class_ "icon" ! src "/svg/state.svg" ! alt "State icon"

statusIcon :: Html
statusIcon = img ! class_ "icon" ! src "/svg/status.svg" ! alt "Status icon"

versionIcon :: Html
versionIcon = img ! class_ "icon" ! src "/svg/version.svg" ! alt "Version icon"

explorerOption :: Bool -> Html
explorerOption isSelected = H.div ! class_ (stringValue ("side-option " ++ if isSelected then "selected-option" else ""))
                                  $ do H.div ! class_ "side-icon"
                                             $ magnifyingGlassIcon
                                       H.div ! class_ "side-text"
                                             $ string "Explorer"

data SyncStatus = Syncing | Synced NominalDiffTime UTCTime | OutOfSync NominalDiffTime UTCTime
  deriving (Eq, Show)

calculateSyncStatus :: UTCTime -> Maybe UTCTime -> SyncStatus
calculateSyncStatus _ Nothing = Syncing
calculateSyncStatus currTime (Just lastRetrievalTime)
  | timeSinceLastSync < 60 = Synced timeSinceLastSync lastRetrievalTime
  | otherwise = OutOfSync timeSinceLastSync lastRetrievalTime
  where
  timeSinceLastSync :: NominalDiffTime
  timeSinceLastSync = currTime `diffUTCTime` lastRetrievalTime

statusLightForTime :: SyncStatus -> Html
statusLightForTime Syncing = amberStatusLight
statusLightForTime (Synced _ _) = greenStatusLight
statusLightForTime (OutOfSync _ _) = redStatusLight

statusExplanationForTime :: SyncStatus -> Html
statusExplanationForTime Syncing = string "Synchronizing"
statusExplanationForTime (Synced _ _) = string "Synchronized"
statusExplanationForTime (OutOfSync _ _) = string "Out of sync"

badgeClass :: SyncStatus -> String
badgeClass Syncing = "syncing-badge"
badgeClass (Synced _ _) = "synced-badge"
badgeClass (OutOfSync _ _) = "out-of-sync-badge"

syncStatus :: SyncStatus -> Html
syncStatus curSyncStatus = H.div ! class_ "side-indicator-wrapper"
                                 $ H.div ! class_ "side-indicator"
                                         $ do H.div ! class_ "status-label"
                                                    $ string "Blockchain status:"
                                              H.div ! class_ (stringValue ("status-badge " ++ badgeClass curSyncStatus))
                                                    $ do H.div ! class_ "status-light"
                                                               $ statusLightForTime curSyncStatus
                                                         H.div ! class_ "status-explantion"
                                                               $ statusExplanationForTime curSyncStatus

baseDoc :: SyncStatus  -> String -> Html -> Html -> Html
baseDoc curSyncStatus titleText caption content = docTypeHtml
                          $ html ! lang "en"
                                 $ do head $ do title $ string titleText
                                                link ! rel "preconnect" ! href "https://fonts.gstatic.com" ! crossorigin
                                                link ! href "https://fonts.googleapis.com/css2?family=Outfit&display=swap" ! rel "stylesheet"
                                                link ! href "/prism/prism.css" ! rel "stylesheet"
                                                link ! href "/prism/marlowe.css" ! rel "stylesheet"
                                                link ! href "/css/stylesheet.css" ! rel "stylesheet"
                                                script ! src "/prism/prism.js" $ mempty
                                                script ! src "/prism/marlowe.js" $ mempty
                                                baseJSScripts
                                      body $ H.div ! class_ "wrapper"
                                                   $ do H.div ! class_ "side-menu"
                                                              $ do fullLogo
                                                                   explorerOption True
                                                                   syncStatus curSyncStatus
                                                        H.div ! class_ "main-content"
                                                              $ do caption
                                                                   content


-- Table for listing contracts (with style)

tableList :: Html -> Html
tableList = H.table ! class_ "table"

tlhr :: Html -> Html
tlhr = H.tr ! class_ "table-header"

tlh :: Html -> Html
tlh = H.th

tlr :: Html -> Html
tlr = H.tr ! class_ "table-row"

tld :: Html -> Html
tld = H.td ! class_ "table-cell"

tldhc :: Html -> Html
tldhc = H.td ! class_ "table-cell hash-cell"

-- Normal basic tables with border

table :: Html -> Html
table = H.table ! style "border: 1px solid black"

tr :: Html -> Html
tr = H.tr

th :: Html -> Html
th = H.th ! style "border: 1px solid black; padding: 5px;"

td :: Html -> Html
td = H.td ! style "border: 1px solid black; padding: 5px;"

-- Adaptors for details table

dtable :: Html -> Html
dtable = H.table ! style "border-spacing: 0 1.8rem;"

dtd :: Html -> Html
dtd = H.td ! class_ "inactive-text flex-container"

mtd :: Html -> Html
mtd = H.td ! style "padding-left: 4rem;"

space :: Html
space = preEscapedString " "

splitLeadingSpaces :: String -> (String, String)
splitLeadingSpaces = span (== ' ')

stringToHtml :: String -> Html
stringToHtml str = mconcat $ map processLine $ lines str
  where
    processLine line = let (spaces, rest) = splitLeadingSpaces line
                       in do mconcat (replicate (length spaces) space)
                             string rest
                             string "\n"

generateLink :: String -> [(String, String)] -> String
generateLink path params = path ++ unpack (renderSimpleQuery True (map (bimap pack pack) params))

prettyPrintAmount :: Int -> Integer -> String
prettyPrintAmount decimalPositions amount = if decimals /= [] then integers ++ '.':decimals else integers
  where (revDecimals, revIntegers) = splitAt decimalPositions $ reverse (show amount)
        decimals = reverse $ padWithZeroesTill decimalPositions revDecimals
        integers = reverse $ addThousandSeparators $ padWithZeroesTill 1 revIntegers

addThousandSeparators :: String -> String
addThousandSeparators (c1:c2:c3:t@(_:_)) = c1:c2:c3:',':addThousandSeparators t
addThousandSeparators r = r

padWithZeroesTill :: Int -> String -> String
padWithZeroesTill x [] = replicate x '0'
padWithZeroesTill x l@(h:t) | x <= 0 = l
                            | otherwise = h:padWithZeroesTill (x - 1) t

formatTimeDiff :: NominalDiffTime -> String
formatTimeDiff diff =
  let rdiff = ceiling diff
      (hours, hrem) = rdiff `divMod` (60 * 60)
      (mins, secs) = hrem `divMod` 60
      hourStr = show hours ++ if hours == 1 then " hour" else " hours"
      minStr = show mins ++ if mins == 1 then " minute" else " minutes"
      secStr = show secs ++ if secs == 1 then " second" else " seconds"
  in case [s | (x, s) <- [(hours, hourStr), (mins, minStr), (secs, secStr)], x /= (0 :: Integer) ] of
       [] -> "just now"
       [x] -> x ++ " ago"
       [x, y] -> x ++ " and " ++ y ++ " ago"
       [x, y, z] -> x ++ ", " ++ y ++ ", and " ++ z ++ " ago"
       _ -> error "A constant list became longer after filtering somehow"

makeLocalDateTime :: UTCTime -> Markup
makeLocalDateTime timestampToRender =
  script ! type_ "text/javascript"
         $ string
  ("(function() {" ++
        "var scripts = document.getElementsByTagName('script');" ++
        "var script = scripts[scripts.length-1];" ++
        "var div = document.createElement('div');" ++
        "div.style = \"display: inline;\";" ++
        "var date = new Date('" ++ show timestampToRender ++ "');" ++
        "div.innerHTML = date.toString().split(' (')[0];" ++
        "script.parentNode.insertBefore(div, script);" ++
      "})();")


linkFor :: ToValue a => a -> String -> Html
linkFor x y = a ! href (toValue x) $ string y

mkTransactionExplorerLink  :: String -> String -> String
mkTransactionExplorerLink  = printf "https://%s/transaction/%s"

mkBlockExplorerLink :: String -> Integer -> String
mkBlockExplorerLink = printf "https://%s/block/%d"

mkTokenPolicyExplorerLink :: String -> String -> String
mkTokenPolicyExplorerLink = printf "https://%s/tokenPolicy/%s"

valueToString :: Value -> String
valueToString = unpack . toStrict . encodePretty

makeTitleDiv :: String -> Html
makeTitleDiv pageTitle = H.div ! class_ "contract-header"
                           $ H.span ! class_ "contract-label"
                                    $ string pageTitle

createPopUpLauncher :: String -> String -> Html -> Html
createPopUpLauncher popupId label popupContent = do
      popUpContent
      a ! class_ "invisible-link"
        ! onclick (toValue showPopUp)
        $ H.span ! class_ "shaded-description-value"
                 $ do string label
                      arrowDropDownIcon
  where (popUpContent, showPopUp) = createPopUp popupId popupContent

createPopUp :: String -> Html -> (Html, String)
createPopUp popUpId content = (popUp, "showPopUp('" ++ popUpId ++ "');")
  where popUp = do H.div ! A.id (toValue $ popUpId ++ "_backdrop")
                         ! onclick (toValue $ "hidePopUp('" ++ popUpId ++ "');")
                         ! class_ "popup-background"
                         $ return ()
                   H.div ! A.id (toValue $ popUpId ++ "_popup")
                         ! class_ "popup-body"
                         $ content

baseJSScripts :: Html
baseJSScripts =
  script ! type_ "text/javascript"
         $ string
             ("function showPopUp(popUpId) {" ++
              " var popUpBackdrop = document.getElementById(popUpId + '_backdrop');" ++
              " popUpBackdrop.style.display = 'block';" ++
              " var popUp = document.getElementById(popUpId + '_popup');" ++
              " popUp.style.display = 'block';" ++
              "}; " ++
              "function hidePopUp(popUpId) {" ++
              " var popUpBackdrop = document.getElementById(popUpId + '_backdrop');" ++
              " popUpBackdrop.style.display = 'none';" ++
              " var popUp = document.getElementById(popUpId + '_popup');" ++
              " popUp.style.display = 'none';" ++
              "}")
