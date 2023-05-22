module Explorer.ResourceHelpers(cssPath, embedResource, svgPath) where

import Data.FileEmbed (embedFile, makeRelativeToProject, embedFile)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Exp)

resourcesPath :: Q FilePath
resourcesPath = makeRelativeToProject "resources/"

cssPath :: Q FilePath
cssPath = (++ "css/") <$> resourcesPath

svgPath :: Q FilePath
svgPath = (++ "svg/") <$> resourcesPath

embedResource :: Q [Char] -> String -> Q Exp
embedResource path name = do 
    path' <- path
    embedFile $ path' ++ name
