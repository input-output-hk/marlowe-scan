{-# LANGUAGE OverloadedStrings #-}

module Scanner.Web.Pagination (PageInfo(..), PageLinkGenerator, bindVal, calcLastPage, calculateRange, renderNavBar) where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html5 (Html, string, (!))

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
calcLastPage :: Int -> Int -> Int
calcLastPage pageLength numContracts = fullPages + partialPages
  where fullPages = numContracts `div` pageLength
        sizeOfPartialPage = numContracts `rem` pageLength
        partialPages = if sizeOfPartialPage > 0 then 1 else 0

data PageInfo = PageInfo {
       currentPage :: Int
     , pageRange :: (Int, Int)
     , totalItems :: Int
     , contractRange :: (Int, Int)
     , numPages :: Int
} deriving (Show, Eq)

type PageLinkGenerator = Int -> Html -> Html

renderNavBar :: PageLinkGenerator -> PageInfo -> Html
renderNavBar plg pinf@(PageInfo { currentPage = page
                                , pageRange = (minPage, maxPage)
                                , numPages = lastPage
                                }) =
  H.div ! class_ "pagination-box"
        $ sequence_ $ [ generateNavLink False plg pinf 1 $ genArrow "<"
                      , generateNavLink True plg pinf (page - 1) $ genLink "Previous"
                      ] ++ [ generateNavLink False plg pinf np $ genLink $ show np | np <- [minPage..maxPage] ] ++
                      [ generateNavLink True plg pinf (page + 1) $ genLink "Next"
                      , generateNavLink False plg pinf lastPage $ genArrow ">"
                      ]

genArrow :: String -> Bool -> Html
genArrow label _ = H.div ! class_ "page-arrow" $ string label

genLink :: String -> Bool -> Html
genLink label isThis = H.div ! class_ (if isThis then "page-button current-page" else "page-button") $ string label

generateNavLink :: Bool -> PageLinkGenerator -> PageInfo -> Int -> (Bool -> Html) -> Html
generateNavLink ommit plg (PageInfo { currentPage = page
                                    , numPages = lastPage
                                    }) targetPage genContent
  
  | page == boundPage && ommit = pure ()
  | otherwise = plg boundPage
                  $ genContent (boundPage == page)
  where boundPage = bindVal 1 lastPage targetPage

