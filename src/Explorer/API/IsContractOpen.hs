module Explorer.API.IsContractOpen(isContractOpen) where
 
import Servant (throwError)
import Opts (Options(..), mkUrlPrefix)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Data.Maybe (isJust)

isContractOpen :: Options -> Maybe [Char] -> IO Bool
isContractOpen _ Nothing = throwError (userError "Need to specify contract id")
isContractOpen opts (Just cid) = do
  let urlPrefix = mkUrlPrefix opts
  ejson <- CJ.getContractJSON urlPrefix cid
  case ejson of
    Left s -> throwError $ userError $ "Could not fetch contract info: " ++ s
    Right CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.currentContract = currContract } } -> return $ isJust currContract

