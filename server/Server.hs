{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

module Main ( main ) where

import           Control.Arrow ((&&&))
import           Data.Char (isAlpha)
import qualified Data.HashMap.Strict as M
import           Data.Proxy
import qualified Data.Text as T
import           Filesystem.Path (basename)
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Protolude hiding (FilePath)
import           Servant.Server
import           Shelly
import           Types

---

newtype Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

newtype Env = Env { posts :: M.HashMap Text Blog }

server :: Env -> Server API
server env = pure (M.elems $ posts env)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

-- | A mapping of word frequencies.
freq :: Text -> M.HashMap Text Int
freq = M.fromList . map ((maybe "死毒殺悪厄魔" identity . head) &&& length) . group . sort . map T.toLower . filter p . T.words . T.map f
  where f c = bool ' ' c $ isAlpha c
        p (T.length -> l) = l > 2 && l < 13

org :: Text -> Sh ()
org f = run_ "emacs" [f, "--batch", "-f", "org-html-export-to-html", "--kill"]

-- | Render all ORG files to HTML, also yielding word frequencies for each file.
orgs :: Sh (M.HashMap Text Blog)
orgs = do
  cd "blog"
  files <- filter (hasExt "org") <$> ls "."
  traverse_ (org . toTextIgnore) files
  M.fromList <$> traverse g files
  where g f = let name = toTextIgnore $ basename f
              in (\t -> (name, Blog name (freq t))) <$> readfile f

main :: IO ()
main = do
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  ps <- shelly orgs
  let prt = maybe 8081 identity p
  putText $ "Listening on port " <> show prt
  W.run prt . app $ Env ps
