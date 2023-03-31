module Explorer.API.IsContractOpen(isContractOpen, isOpenAJAXBox) where
 
import Servant (throwError)
import Opts (Options(..), mkUrlPrefix)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Data.Maybe (isJust)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Explorer.Web.Util (generateLink)

isContractOpen :: Options -> Maybe [Char] -> IO Bool
isContractOpen _ Nothing = throwError (userError "Need to specify contract id")
isContractOpen opts (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  ejson <- CJ.getContractJSON urlPrefix cid
  case ejson of
    Left s -> throwError $ userError $ "Could not fetch contract info: " ++ s
    Right CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.currentContract = currContract } } -> return $ isJust currContract

isOpenAJAXBox :: String -> Html
isOpenAJAXBox cid = do
    let url = generateLink "isContractOpen" [("contractId", cid)]
        divId = "isOpen_" ++ cid

    H.div ! A.id (H.toValue divId) $ H.string "Loading..."

    H.script ! A.type_ (H.toValue "text/javascript") $ H.string $
        "function isContractOpen() {\n" ++
        "  var xhr = new XMLHttpRequest();\n" ++
        "  xhr.onreadystatechange = function() {\n" ++
        "    if (xhr.readyState == 4) {\n" ++ 
        "      if (xhr.status == 200) {\n" ++
        "        document.getElementById('" ++ divId ++ "').innerHTML = xhr.responseText;\n" ++
        "      } else {\n" ++
        "        document.getElementById('" ++ divId ++ "').innerHTML = 'Error';\n" ++
        "      }\n" ++
        "    }\n" ++
        "  };\n" ++
        "  xhr.open('GET', '" ++ url ++ "', true);\n" ++
        "  xhr.send();\n" ++
        "}\n" ++
        "isContractOpen();\n"
