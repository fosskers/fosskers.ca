{-# LANGUAGE DataKinds, TypeOperators, Rank2Types #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main ( main ) where

import           Common
import           Control.Arrow ((&&&))
import           Control.Lens hiding (index)
import           Data.Char (isAlpha)
import qualified Data.HashMap.Strict as M
import           Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import           Filesystem.Path (basename)
import           Lucid
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Generic
import           Org (parseOrg)
import           Protolude hiding (FilePath)
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           Shelly hiding (path)
import           System.Environment (lookupEnv)

---

data Args = Args { port :: Maybe Int <?> "Port to listen for requests on, otherwise $PORT"
                 , js   :: Text      <?> "Javascript bundle to serve" }
  deriving (Generic, ParseRecord)

data Env = Env { stats :: [Blog], posts :: M.HashMap Text (Html ()), bundle :: Text }

server :: Env -> Server API
server env = pure (stats env)
  :<|> (\p -> maybe (throwError err404) pure . M.lookup p $ posts env)
  :<|> serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> pure (index $ bundle env)

app :: Env -> Application
app = gzip (def { gzipFiles = GzipCompress }) . serve (Proxy :: Proxy API) . server

index :: Text -> Html ()
index j = html_ $ head_ h *> body_ (script_ [src_ $ "assets/" <> j] ("" :: Text))
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
                , href_ "assets/fa-solid.min.css" ]
          link_ [ rel_ "stylesheet"
                , href_ "assets/fosskers.css" ]

-- | A mapping of word frequencies.
freq :: Text -> [(Text, Int)]
freq = map ((maybe "死毒殺悪厄魔" identity . head) &&& length) . group . sort . filter g . map T.toLower . T.words . T.map f
  where f c = bool ' ' c $ isAlpha c
        g w = let l = T.length w in l > 2 && l < 13 && not (S.member w functionWords)

htmlPath :: Text -> Text
htmlPath path = T.dropEnd 4 path <> ".html"

functionWords :: S.Set Text
functionWords = S.fromList [ "and", "but", "for", "our", "the", "that", "this", "these", "those", "then", "than"
                           , "what", "when", "where", "will", "your", "you", "are", "can", "has", "have"
                           , "here", "there", "how", "who", "its", "just", "not", "now", "only", "they" ]

-- | Render all ORG files to HTML, also yielding word frequencies for each file.
--
-- Can assume:
--   - Every English article has a Japanese analogue
--   - The true title always appears on the first line of the file
--   - The original (ballpark) date of writing is on the second line of the "base" file
orgs :: Sh ([Text], [Blog], M.HashMap Text (Html ()))
orgs = do
  cd "blog"
  files <- filter (T.isSuffixOf ".org") <$> lsT "."
  vs <- traverse g files
  let (errs, (blogs, pairs)) = second unzip $ partitionEithers vs
  pure (errs, blogs, M.fromList pairs)
  where g :: Text -> Sh (Either Text (Blog, (Text, Html ())))
        g f = do
          let path = fromText f
          content <- eread path
          html    <- eread . fromText $ htmlPath f
          pure $ do
            c <- content
            h <- html
            let base = toTextIgnore $ basename path
            (t, d) <- parseOrg f c
            pure (Blog t d (Path base) (freq c), (base, toHtmlRaw h))

-- TODO I don't like the way this feels/looks
eread :: FilePath -> Sh (Either Text Text)
eread path = do
  exists <- test_f path
  if exists
     then Right <$> readfile path
     else pure . Left $ toTextIgnore path <> " doesn't exist to be read"

main :: IO ()
main = do
  Args (Helpful p) (Helpful j) <- getRecord "Backend server for fosskers.ca"
  (errs, ps, hs) <- shelly orgs
  traverse_ putText errs
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  let prt = maybe 8081 identity $ p <|> herokuPort
  putText $ "Listening on port " <> show prt
  W.run prt . app $ Env ps hs j
