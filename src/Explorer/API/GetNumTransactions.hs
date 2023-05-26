module Explorer.API.GetNumTransactions(getContractNumTransactions, numTransactionsAJAXBox) where

import Servant (throwError)
import Opts (Options(..), mkUrlPrefix)
import qualified Language.Marlowe.Runtime.Types.TransactionsJSON as TJs
import Data.List (genericLength)
import Text.Blaze.Html5 (Html, (!))
import Explorer.Web.Util (generateLink)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

getContractNumTransactions :: Options -> Maybe [Char] -> IO Integer
getContractNumTransactions _ Nothing = throwError (userError "Need to specify contract id")
getContractNumTransactions opts (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  ejson <- TJs.getContractTransactionsByContractId urlPrefix cid
  case ejson of
    Left s -> throwError $ userError $ "Could not fetch contract transactions: " ++ s
    Right TJs.Transactions { TJs.transactions = tList } -> return $ genericLength tList

numTransactionsAJAXBox :: String -> Html
numTransactionsAJAXBox cid = do
    let url = generateLink "getNumTransactions" [("contractId", cid)]
        divId = "numTransFor_" ++ cid

    H.div ! A.id (H.toValue divId) $ H.string "..."

    H.script ! A.type_ (H.toValue "text/javascript") $ H.string $
        "function getNumTransactions() {\n" ++
        "  var xhr = new XMLHttpRequest();\n" ++
        "  xhr.onreadystatechange = function() {\n" ++
        "    if (xhr.readyState == 4) {\n" ++ 
        "      if (xhr.status == 200) {\n" ++
        "        document.getElementById('" ++ divId ++ "').innerHTML = xhr.responseText;\n" ++
        "      } else {\n" ++
        "        document.getElementById('" ++ divId ++ "').innerHTML = 'Err';\n" ++
        "      }\n" ++
        "    }\n" ++
        "  };\n" ++
        "  xhr.open('GET', '" ++ url ++ "', true);\n" ++
        "  xhr.send();\n" ++
        "}\n" ++
        "getNumTransactions();\n"
