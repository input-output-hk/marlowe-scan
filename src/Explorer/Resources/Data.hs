{-# LANGUAGE TemplateHaskell #-}
module Explorer.Resources.Data(cssStylesheet, activeLight, greenStatus, inactiveLight, logo, magnifyingGlass, amberStatus, redStatus, downloadIcon, blockHeaderHashIcon, blockNoIcon, contractIdIcon, metadataIcon, roleTokenMintingPolicyIdIcon, slotNoIcon, statusIcon, versionIcon, prismCSS, prismJS, marlowePrismJS, marlowePrismCSS, stateIcon, arrowDropDown, alarmClock) where

import Explorer.Resources.Helpers (cssPath, embedResource, svgPath, prismPath)
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

alarmClock :: ByteString
alarmClock = $(embedResource svgPath "alarm_clock.svg")

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

arrowDropDown :: ByteString
arrowDropDown = $(embedResource svgPath "arrow_drop_down.svg")

blockHeaderHashIcon :: ByteString
blockHeaderHashIcon = $(embedResource svgPath "block_header_hash.svg")

blockNoIcon :: ByteString
blockNoIcon = $(embedResource svgPath "block_no.svg")

contractIdIcon :: ByteString
contractIdIcon = $(embedResource svgPath "contract_id.svg")

metadataIcon :: ByteString
metadataIcon = $(embedResource svgPath "metadata.svg")

roleTokenMintingPolicyIdIcon :: ByteString
roleTokenMintingPolicyIdIcon = $(embedResource svgPath "role_token_minting_policy_id.svg")

slotNoIcon :: ByteString
slotNoIcon = $(embedResource svgPath "slot_no.svg")

statusIcon :: ByteString
statusIcon = $(embedResource svgPath "status.svg")

stateIcon :: ByteString
stateIcon = $(embedResource svgPath "state.svg")

versionIcon :: ByteString
versionIcon = $(embedResource svgPath "version.svg")

prismCSS :: ByteString
prismCSS = $(embedResource prismPath "prism.css")

prismJS :: ByteString
prismJS = $(embedResource prismPath "prism.js")

marlowePrismJS :: ByteString
marlowePrismJS = $(embedResource prismPath "marlowe.js")

marlowePrismCSS :: ByteString
marlowePrismCSS = $(embedResource prismPath "marlowe.css")
