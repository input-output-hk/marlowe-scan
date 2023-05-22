{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Explorer.Resources.MimeTypes(CSS, SVG) where

import Network.HTTP.Media ((//), (/:), MediaType)
import Servant (Accept (..), Proxy, MimeRender (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data CSS

instance Accept CSS where
  contentType :: Proxy CSS -> MediaType
  contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender CSS BS.ByteString where
  mimeRender :: Proxy CSS -> BS.ByteString -> LBS.ByteString
  mimeRender _ = LBS.fromStrict

data SVG

instance Accept SVG where
  contentType :: Proxy SVG -> MediaType
  contentType _ = "image" // "svg+xml" /: ("charset", "utf-8")

instance MimeRender SVG BS.ByteString where
  mimeRender :: Proxy SVG -> BS.ByteString -> LBS.ByteString
  mimeRender _ = LBS.fromStrict

