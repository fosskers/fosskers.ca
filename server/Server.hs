{-# LANGUAGE DataKinds, TypeOperators, Rank2Types #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}

module Main ( main ) where

import           Common
import           Control.Arrow ((&&&))
import           Control.Lens hiding (index)
import           Data.Char (isAlpha)
import qualified Data.HashMap.Strict as M
import           Data.Proxy
import qualified Data.Text as T
import           Filesystem.Path (basename)
import           Lucid
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Org (parseEng, parseJap)
import           Protolude hiding (FilePath)
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           Shelly hiding (path)
import qualified Text.Blaze.Html as B

---

newtype Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

data Env = Env { stats :: [Blog], posts :: M.HashMap Text B.Html }

server :: Env -> Server API
server env = pure (stats env)
  :<|> (\p -> maybe (throwError err404) pure . M.lookup p $ posts env)
  :<|> serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> pure xhrtest
  :<|> pure index

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

index :: Html ()
index = html_ $ head_ h *> body_ (script_ [src_ "assets/app.js"] ("" :: Text))
  where h = do
          title_ "fosskers.ca"
          meta_ [ charset_ "utf-8" ]
          meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
          link_ [ rel_ "stylesheet"
                , href_ "assets/bootstrap.min.css" ]
          link_ [ rel_ "stylesheet"
                , href_ "assets/fontawesome.min.css" ]
          link_ [ rel_ "stylesheet"
                , href_ "assets/fa-brands.min.css" ]
          link_ [ rel_ "stylesheet"
                , href_ "assets/fosskers.css" ]

xhrtest :: Html ()
xhrtest = div_ "XHR Test successful!" *> div_ "Oh dang, another one"

-- | A mapping of word frequencies.
freq :: Text -> [(Text, Int)]
freq = map ((maybe "死毒殺悪厄魔" identity . head) &&& length) . group . sort . map T.toLower . filter g . T.words . T.map f
  where f c = bool ' ' c $ isAlpha c
        g (T.length -> l) = l > 2 && l < 13

org :: Text -> Sh ()
org f = run_ "emacs" [f, "--batch", "-f", "org-html-export-to-html", "--kill"]

-- | The expected filepath of the Japanese version of some blog post,
-- given its full English filepath.
jPath :: Text -> Text
jPath path = T.dropEnd 4 path <> "-jp.org"

htmlPath :: Text -> Text
htmlPath path = T.dropEnd 4 path <> ".html"

-- | Render all ORG files to HTML, also yielding word frequencies for each file.
--
-- Can assume:
--   - Every English article has a Japanese analogue
--   - The true title always appears on the first line of the file
--   - The original (ballpark) date of writing is on the second line of the "base" file
orgs :: Sh ([Text], [Blog], M.HashMap Text B.Html)
orgs = do
  cd "blog"
  files <- filter (T.isSuffixOf ".org") <$> lsT "."
  traverse_ org files
  vs <- traverse g $ filter (not . T.isSuffixOf "-jp.org") files
  let (errs, (blogs, pairs)) = second unzip $ partitionEithers vs
  pure $ (errs, blogs, M.fromList $ concat pairs)
  where g :: Text -> Sh (Either Text (Blog, [(Text, B.Html)]))
        g f = do
          let engPath = fromText f
              japPath = fromText $ jPath f
          engContent <- eread engPath
          japContent <- eread . fromText $ jPath f
          engHtml    <- eread . fromText $ htmlPath f
          japHtml    <- eread . fromText . htmlPath $ jPath f
          pure $ do
            engC <- engContent
            japC <- japContent
            engH <- engHtml
            japH <- japHtml
            let ebase = toTextIgnore $ basename engPath
                jbase = toTextIgnore $ basename japPath
            blog <- h ebase engC japC
            let html = [(ebase, engH), (jbase, japH)] & traverse . _2 %~ B.preEscapedToHtml
            pure (blog, html)

        h :: Text -> Text -> Text -> Either Text Blog
        h fname eng jap = (\(et,d) jt -> Blog et jt d (Path fname) (freq eng)) <$> parseEng eng <*> parseJap jap

-- TODO I don't like the way this feels/looks
eread :: FilePath -> Sh (Either Text Text)
eread path = do
  exists <- test_f path
  if exists
     then Right <$> readfile path
     else pure . Left $ toTextIgnore path <> " doesn't exist to be read"

main :: IO ()
main = do
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  (errs, ps, hs) <- shelly orgs
  traverse_ putText errs
  let prt = maybe 8081 identity p
  putText $ "Listening on port " <> show prt
  W.run prt . app $ Env ps hs
