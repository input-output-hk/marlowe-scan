{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Util
  ( baseDoc, formatTimeDiff, generateLink, linkFor, makeLocalDateTime, prettyPrintAmount, stringToHtml, table, td, th, tr, mkTransactionExplorerLink , mkBlockExplorerLink, mkTokenPolicyExplorerLink, valueToString, tableList, tlh, tlhr, tlr, tld )
  where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 ( pack, unpack )
import Network.HTTP.Types ( renderSimpleQuery )
import Prelude hiding ( head )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( body, docTypeHtml, h1, head, html, title,
                          string, Html, (!), br, preEscapedString, a, ToValue (toValue), Markup, script, link, customAttribute, img, stringValue )
import Text.Blaze.Html5.Attributes ( style, lang, href, type_, rel, class_, src, alt )
import Data.Time (UTCTime, NominalDiffTime)
import Text.Printf (printf)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)

crossorigin :: H.Attribute
crossorigin = customAttribute "crossorigin" ""

logo :: Html
logo = img ! src "/svg/logo.svg" ! alt "Marlowe logo" ! class_ "logo-pic"

magnifyingGlassIcon :: Html
magnifyingGlassIcon = img ! src "/svg/magnifying-glass.svg" ! alt "Magnifying glass" ! class_ "side-icon"

fullLogo :: Html
fullLogo = H.div ! class_ "logo"
--               $ a ! href "/"
                 $ do logo
                      H.div ! class_ "logo-text"
                            $ do H.span ! class_ "logo-text-marlowe" $ string "Marlowe"
                                 nbsp
                                 H.span ! class_ "logo-text-explorer" $ string "Explorer"

explorerOption :: Bool -> Html
explorerOption isSelected = H.div ! class_ (stringValue ("side-option " ++ if isSelected then "selected-option" else ""))
                                  $ do H.div ! class_ "side-icon"
                                             $ magnifyingGlassIcon
                                       H.div ! class_ "side-text"
                                             $ string "Explorer"

baseDoc :: String -> Html -> Html
baseDoc caption content = docTypeHtml
                          $ html ! lang "en"
                                 $ do head $ do title $ string caption
                                                link ! rel "preconnect" ! href "https://fonts.gstatic.com" ! crossorigin
                                                link ! href "https://fonts.googleapis.com/css2?family=Outfit&display=swap" ! rel "stylesheet"
                                                link ! href "/css/stylesheet.css" ! rel "stylesheet"
                                      body $ H.div ! class_ "wrapper"
                                                   $ do H.div ! class_ "side-menu"
                                                              $ do fullLogo
                                                                   explorerOption True
                                                        H.div ! class_ "main-content"
                                                              $ do h1 $ string caption
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

-- Normal basic tables with border

table :: Html -> Html
table = H.table ! style "border: 1px solid black"

tr :: Html -> Html
tr = H.tr

th :: Html -> Html
th = H.th ! style "border: 1px solid black; padding: 5px;"

td :: Html -> Html
td = H.td ! style "border: 1px solid black; padding: 5px;"

nbsp :: Html
nbsp = preEscapedString "&nbsp;"

splitLeadingSpaces :: String -> (String, String)
splitLeadingSpaces = span (== ' ')

stringToHtml :: String -> Html
stringToHtml str = mconcat $ map processLine $ lines str
  where
    processLine line = let (spaces, rest) = splitLeadingSpaces line
                       in do mconcat (replicate (length spaces) nbsp)
                             string rest
                             br

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
