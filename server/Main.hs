{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Main ( main ) where

import           BasePrelude hiding (app, option)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Binary.Builder (toLazyByteString)
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Org as O
import qualified Data.Org.Lucid as O
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import           Fosskers.Common
import           Fosskers.Site
import           Fosskers.Site.About (about)
import           Fosskers.Site.Blog (blog, choose, newest)
import           Fosskers.Site.CV (cv)
import           Lucid
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Applicative hiding (style)
import           Skylighting hiding (formatHtmlBlock)
import           Skylighting.Format.HTML.Lucid (formatHtmlBlock)
import           System.Directory (doesFileExist, listDirectory, makeAbsolute)
import           System.FilePath ((</>))
import           Text.Megaparsec (errorBundlePretty, parse)
import           Xmlbf (ToXml(..), encode)

---

newtype Args = Args (Maybe Int)

pArgs :: Parser Args
pArgs = Args
  <$> optional (option auto $ long "port" <> help "Port to listen on, otherwise $PORT")

html :: Html () -> Response
html = responseLBS status200 [("Content-Type", "text/html")] . renderBS

xml :: ToXml a => a -> Response
xml = responseLBS status200 headers . toLazyByteString . encode . toXml
  where
    headers = [("Content-Type", "application/xml")]

rss :: Blogs -> Language -> ByLanguage
rss bs l = ByLanguage $ NEL.sortWith (Down . orgDate . blogRaw) ps
  where
    ps = case l of
      English  -> engByCat bs >>= bcBlogs
      Japanese -> japByCat bs >>= bcBlogs

-- | Compress all responses (especially assets!) coming out of the server.
compress :: Application -> Application
compress = gzip (def { gzipFiles = GzipCompress })

app :: Pages -> Blogs -> Application
app ps bs = compress routes
  where
    routes :: Application
    routes req resp = case pathInfo req of
      -- Assets --
      "assets" : rest -> assets (req { pathInfo = rest }) resp
      "webfonts" : rest -> assets (req { pathInfo = rest }) resp
      [ "favicon.ico" ] -> assets req resp
      -- Static pages --
      [ lang, "about" ] -> resp $ withLang lang (\l -> html . site l About $ about ps l)
      [ lang, "cv" ] -> resp $ withLang lang (\l -> html . site l CV $ cv ps l)
      -- All blog posts --
      [ lang, "blog" ] ->
        resp $ withLang lang (\l -> html . site l Posts . blog bs l . Just $ newest bs l)
      [ lang, "blog", slug ] ->
        resp $ withLang lang (\l -> html . site l Posts . blog bs l $ choose bs l slug)
      -- RSS feed --
      [ lang, "rss" ] -> resp $ withLang lang (xml . rss bs)
      -- The language button --
      [ lang ] ->
        resp $ withLang lang (\l -> html . site l Posts . blog bs l . Just $ newest bs l)
      -- Index page yields most recent English blog post --
      [] -> resp . html . site English Posts . blog bs English . Just $ newest bs English
      _ -> resp err404

    err404 :: Response
    err404 = responseLBS status404 [("Content-Type", "text/html")]
      . renderBS $ site English Nowhere nowhere

    assets :: Application
    assets = staticApp (defaultFileServerSettings "assets")

    withLang :: Text -> (Language -> Response) -> Response
    withLang "en" f = f English
    withLang "jp" f = f Japanese
    withLang _ _    = err404

-- | Syntax highlighting "middleware" thanks to Skylighting and org-mode-lucid.
skylighting :: O.Highlighting
skylighting l t = maybe (O.codeHTML l t) (formatHtmlBlock fo) $ do
  O.Language lang <- l <|> Just (O.Language "Default")
  syn <- syntaxByName defaultSyntaxMap lang
  hush $ tokenize tc syn t
  where
    tc = TokenizerConfig defaultSyntaxMap False
    fo = defaultFormatOpts
      { containerClasses = "src" : maybe [] (\(O.Language l') -> ["src-" <> l']) l }

sectioning :: O.SectionStyling
sectioning _ h s = do
  h
  div_ [style_ "padding-bottom: 1.0%;padding-left: 2.0%"] s

-- | Abosolute paths to all the @.org@ blog files.
orgFiles :: IO [FilePath]
orgFiles = filter (L.isSuffixOf ".org") <$> (listDirectory "blog" >>= traverse f)
  where
    f fp = makeAbsolute $ "blog" </> fp

-- | Render all ORG files to HTML.
orgs :: NonEmpty FilePath -> IO ([Text], [Blog])
orgs = fmap partitionEithers . traverse g . NEL.toList
  where
    style :: O.OrgStyle
    style = O.OrgStyle False (Just $ O.TOC "Contents" 2) True skylighting sectioning (Just ' ')

    g :: FilePath -> IO (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang f
        void . note ("No date provided for: " <> path) $ orgDate ofile
        void . note ("No title provided for: " <> path) . M.lookup "TITLE" $ O.orgMeta ofile
        Right . Blog lang (pathSlug f) ofile $ O.body style ofile

eread :: FilePath -> IO (Either Text Text)
eread path = do
  exists <- doesFileExist path
  if exists
     then Right . T.decodeUtf8With lenientDecode <$> B.readFile path
     else pure . Left $ T.pack path <> " doesn't exist to be read."

pages :: IO (Maybe Pages)
pages = runMaybeT $ Pages
  <$> f astyle "org/about-en.org"
  <*> f astyle "org/about-jp.org"
  <*> f cstyle "org/cv-en.org"
  <*> f jstyle "org/cv-jp.org"
  where
    astyle = O.OrgStyle False Nothing False skylighting sectioning (Just ' ')
    cstyle = O.OrgStyle True (Just $ O.TOC "Index" 2) True skylighting sectioning (Just ' ')
    jstyle = O.OrgStyle True (Just $ O.TOC "目次" 2) True skylighting sectioning (Just ' ')

    f :: O.OrgStyle -> FilePath -> MaybeT IO (Html ())
    f s fp = O.body s <$> MaybeT (orgd fp)

orgd :: FilePath -> IO (Maybe O.OrgFile)
orgd fp = (hush >=> O.org) <$> eread fp

main :: IO ()
main = do
  args <- execParser opts
  setup args
  where
    opts = info (pArgs <**> helper) (fullDesc <> header "Server for fosskers.ca")

setup :: Args -> IO ()
setup args = f >>= \case
  Left err -> logError err >> exitFailure
  Right (ps, ens, jps) -> work args ps ens jps
  where
    f :: IO (Either Text (Pages, NonEmpty Blog, NonEmpty Blog))
    f = runExceptT $ do
      ps <- noteT "Couldn't read static pages." $ MaybeT pages
      fs <- noteT "No .org files were found." . MaybeT $ fmap NEL.nonEmpty orgFiles
      (bads, goods) <- liftIO $ orgs fs
      liftIO $ traverse_ logWarn bads
      (ens, jps) <- splitLs goods ?? "Missing posts from Japanese or English."
      pure (ps, ens, jps)

splitLs :: [Blog] -> Maybe (NonEmpty Blog, NonEmpty Blog)
splitLs = bitraverse NEL.nonEmpty NEL.nonEmpty . partition (\b -> blogLang b == English)

work :: Args -> Pages -> NonEmpty Blog -> NonEmpty Blog -> IO ()
work (Args p) ps ens jps = do
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  cores <- getNumCapabilities
  let !prt = fromMaybe 8081 $ p <|> herokuPort
      !eng = mapify ens
      !jap = mapify jps
      !newEs = sortByDate ens
      !newJs = sortByDate jps
      !bls = Blogs (byCats newEs) (byCats newJs) (NEL.head newEs) (NEL.head newJs) eng jap
  logInfo . T.pack $ printf "Blog posts read: %d" (length ens + length jps)
  logInfo . T.pack $ printf "Listening on port %d with %d cores" prt cores
  W.run prt $ app ps bls

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.reverse . NEL.sortWith (orgDate . blogRaw)

byCats :: NonEmpty Blog -> NonEmpty BlogCategory
byCats = NEL.map toCat . NEL.groupWith1 fst . NEL.sortWith fst . NEL.map (cat &&& id)
  where
    cat :: Blog -> Text
    cat = maybe "Misc." T.toTitle . M.lookup "CATEGORY" . O.orgMeta . blogRaw

    toCat :: NonEmpty (Text, Blog) -> BlogCategory
    toCat a@((c, _) :| _) = BlogCategory c $ NEL.map snd a

mapify :: NonEmpty Blog -> Map Text Blog
mapify = M.fromList . NEL.toList . NEL.map (blogSlug &&& id)

logError :: Text -> IO ()
logError t = T.putStrLn $ "[error] " <> t

logWarn :: Text -> IO ()
logWarn t = T.putStrLn $ "[warn] " <> t

logInfo :: Text -> IO ()
logInfo t = T.putStrLn $ "[info] " <> t
