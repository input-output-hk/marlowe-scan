module Scanner.API.IsContractOpen(isContractOpen, isOpenAJAXBox) where
 
import Servant (throwError)
import Opts (Options(..), mkUrlPrefix)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Data.Maybe (isJust)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Scanner.Web.Util (generateLink)

isContractOpen :: Options -> Maybe [Char] -> IO Bool
isContractOpen _ Nothing = throwError (userError "Need to specify contract id")
isContractOpen opts (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  ejson <- CJ.getContractJSON urlPrefix cid
  case ejson of
    Left s -> throwError $ userError $ "Could not fetch contract info: " ++ s
    Right CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.currentContract = currContract } } -> return $ isJust currContract

activeLight :: String
activeLight = "var activeLightDiv = document.createElement('div');\n" ++
              "activeLightDiv.setAttribute('class', 'active-light');\n" ++
              "var activeLightImg = document.createElement('img');\n" ++
              "activeLightImg.setAttribute('src', '/svg/active-light.svg');\n" ++
              "activeLightImg.setAttribute('alt', 'Open contract');\n" ++
              "activeLightDiv.appendChild(activeLightImg);\n" ++
              "node.appendChild(activeLightDiv);\n"

inactiveLight :: String
inactiveLight = "var inactiveLightDiv = document.createElement('div');\n" ++
                "inactiveLightDiv.setAttribute('class', 'active-light');\n" ++
                "var inactiveLightImg = document.createElement('img');\n" ++
                "inactiveLightImg.setAttribute('src', '/svg/inactive-light.svg');\n" ++
                "inactiveLightImg.setAttribute('alt', 'Closed contract');\n" ++
                "inactiveLightDiv.appendChild(inactiveLightImg);\n" ++
                "node.appendChild(inactiveLightDiv);\n"

isOpenAJAXBox :: String -> Html
isOpenAJAXBox cid = do
    let url = generateLink "isContractOpen" [("contractId", cid)]
        divId = "isOpen_" ++ cid

    H.div ! A.id (H.toValue divId) $ H.string "..."

    H.script ! A.type_ (H.toValue "text/javascript") $ H.string $
        "function isContractOpen() {\n" ++
        "  var xhr = new XMLHttpRequest();\n" ++
        "  xhr.onreadystatechange = function() {\n" ++
        "    if (xhr.readyState == 4) {\n" ++ 
        "      if (xhr.status == 200) {\n" ++
        "        var node = document.getElementById('" ++ divId ++ "');\n" ++
        "        node.innerHTML = '';\n" ++ 
        "        if (xhr.responseText == 'true') {\n" ++ activeLight ++
        "          node.parentNode.parentNode.classList.add('active');\n" ++
        "        } else {\n" ++ inactiveLight ++
        "        }\n" ++
        "      } else {\n" ++
        "        document.getElementById('" ++ divId ++ "').innerHTML = 'Err';\n" ++
        "      }\n" ++
        "    }\n" ++
        "  };\n" ++
        "  xhr.open('GET', '" ++ url ++ "', true);\n" ++
        "  xhr.send();\n" ++
        "}\n" ++
        "isContractOpen();\n"
