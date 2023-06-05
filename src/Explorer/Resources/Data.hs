{-# LANGUAGE TemplateHaskell #-}
module Explorer.Resources.Data(cssStylesheet, activeLight, greenStatus, inactiveLight, logo, magnifyingGlass, amberStatus, redStatus, downloadIcon) where

import Explorer.Resources.Helpers (cssPath, embedResource, svgPath)
import Data.ByteString (ByteString)

----------------
-- Stylesheet --
----------------

cssStylesheet :: ByteString
cssStylesheet = $(embedResource cssPath "stylesheet.css")

----------
-- SVGs --
----------

activeLight :: ByteString
activeLight = $(embedResource svgPath "active-light.svg")

greenStatus :: ByteString
greenStatus = $(embedResource svgPath "green-status-light.svg")

amberStatus :: ByteString
amberStatus = $(embedResource svgPath "amber-status-light.svg")

redStatus :: ByteString
redStatus = $(embedResource svgPath "red-status-light.svg")

inactiveLight :: ByteString
inactiveLight = $(embedResource svgPath "inactive-light.svg")

logo :: ByteString
logo = $(embedResource svgPath "logo.svg")

magnifyingGlass :: ByteString
magnifyingGlass = $(embedResource svgPath "magnifying-glass.svg")

downloadIcon :: ByteString
downloadIcon = $(embedResource svgPath "download.svg")
