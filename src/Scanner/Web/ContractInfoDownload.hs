{-# LANGUAGE DataKinds #-}
module Scanner.Web.ContractInfoDownload(contractDownloadInfo) where

import Data.Aeson (encode)
import Codec.Archive.Zip (toEntry, addEntryToArchive, emptyArchive, fromArchive)
import Servant (throwError, addHeader, Headers, Header)
import qualified Language.Marlowe.Runtime.Types.ContractJSON as CJ
import Opts ( mkUrlPrefix, Options )
import Data.ByteString.Lazy (ByteString)
import Data.Time (getCurrentTime)
import Data.List (foldl')
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


data DownloadError = JSONFetchingError String
                   | NoContractId
    deriving Show

contractDownloadInfo :: Options -> Maybe String -> IO (Headers '[Header "Content-Disposition" String] ByteString)
contractDownloadInfo opts (Just cid) = do
    let urlPrefix = mkUrlPrefix opts
    currPOSIX <- utcTimeToPOSIXSeconds <$> getCurrentTime
    eCJSON <- CJ.getContractJSON urlPrefix cid
    case eCJSON of
      Left err -> throwError (userError err)
      Right (CJ.ContractJSON { CJ.resource = CJ.Resource { CJ.initialContract = contract
                                                         , CJ.metadata = metadata'
                                                         , CJ.state = state'
                                                         }
                             }) -> let files = [ ("contract.json", encode contract)
                                               , ("metadata.json", encode metadata')
                                               , ("state.json", encode state')
                                               ]
                                       zipEntries = [toEntry fileName (round currPOSIX) bytes | (fileName, bytes) <- files]
                                       archive = foldl' (flip addEntryToArchive) emptyArchive zipEntries
                                       contentDisposition = "attachment; filename=\"contract_info.zip\""
                                   in
                                   return $ addHeader contentDisposition $ fromArchive archive
contractDownloadInfo _opts Nothing = throwError (userError "Need to specify contract id")
