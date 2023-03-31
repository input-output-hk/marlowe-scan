module Explorer.API.GetNumTransactions(getContractNumTransactions) where

import Servant (throwError)
import Opts (Options(..), mkUrlPrefix)
import qualified Language.Marlowe.Runtime.Types.TransactionsJSON as TJs
import Data.List (genericLength)

getContractNumTransactions :: Options -> Maybe [Char] -> IO Integer
getContractNumTransactions _ Nothing = throwError (userError "Need to specify contract id")
getContractNumTransactions opts (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  ejson <- TJs.getContractTransactionsByContractId urlPrefix cid
  case ejson of
    Left s -> throwError $ userError $ "Could not fetch contract transactions: " ++ s
    Right TJs.Transactions { TJs.transactions = tList } -> return $ genericLength tList
