{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Web.PrettyHtml(PrettyHtml) where
import Servant (Accept (..), MimeRender (..), Proxy)
import Text.Blaze (ToMarkup)
import Text.Blaze.Html (toHtml, Html)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Network.HTTP.Media (MediaType, (//), (/:))
import Codec.Binary.UTF8.Generic (fromString)

data PrettyHtml

instance Accept PrettyHtml where
    contentType :: Proxy PrettyHtml -> MediaType
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender PrettyHtml a where
    mimeRender :: Proxy PrettyHtml -> a -> ByteString
    mimeRender _ =  fromString . renderHtml . toHtml

instance MimeRender PrettyHtml Html where
    mimeRender :: Proxy PrettyHtml -> Html -> ByteString
    mimeRender _ = fromString . renderHtml
