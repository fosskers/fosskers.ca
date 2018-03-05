{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Fosskers.XML where

import           Data.ByteString.Builder (toLazyByteString)
import qualified Network.HTTP.Media as M
import           Protolude
import           Servant.API
import           Xmlbf

---

data XML

instance Accept XML where
  contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance ToXml a => MimeRender XML a where
  mimeRender _ = toLazyByteString . encode . toXml
