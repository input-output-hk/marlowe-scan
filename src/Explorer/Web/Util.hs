{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Util
  ( baseDoc, mkNavLink, stringToHtml, table, td, th, tr )
  where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 ( pack, unpack )
import Network.HTTP.Types ( renderSimpleQuery )
import Prelude hiding ( head )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( body, docTypeHtml, h1, head, html, title,
                          string, Html, (!), br, preEscapedString, a, ToValue (toValue) )
import Text.Blaze.Html5.Attributes ( style, lang, href )

baseDoc :: String -> Html -> Html
baseDoc caption content = docTypeHtml
                          $ html ! lang "en"
                                 $ do head $ title $ string caption
                                      body $ do h1 $ string caption
                                                content

mkNavLink :: Bool -> String -> String -> String -> Html
mkNavLink True _ _ tabTitle =
  td $ string tabTitle
mkNavLink False cid tabName tabTitle =
  td $ a ! href (toValue $ generateLink "contractView" [("tab", tabName), ("contractId", cid)])
         $ string tabTitle

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
