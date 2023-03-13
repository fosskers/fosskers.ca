{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Main ( main ) where

import           BasePrelude hiding (app)
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
import           Data.Time (Day, UTCTime(..), getCurrentTime, toGregorian)
import           Fosskers.Common
import           Fosskers.Site
import           Fosskers.Site.About (about)
import           Fosskers.Site.AlBhed (alBhed)
import           Fosskers.Site.Blog (blog, choose, newest)
import           Fosskers.Site.CV (cv)
import           Fosskers.Site.Drj (drj)
import           Fosskers.Site.GameOfLife (gol)
import           Fosskers.Site.Landing (landing)
import           Fosskers.Site.Love (love)
import           Fosskers.Site.SeedEffects (seedEffects)
import           Fosskers.Site.Twitch (twitch)
import           Fosskers.Site.WebEffects (webEffects)
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
rss bs l = ByLanguage $ NEL.sortWith (Down . blogDate) ps
  where
    ps = case l of
      English  -> engByCat bs >>= bcBlogs
      Japanese -> japByCat bs >>= bcBlogs

-- | Compress all responses (especially assets!) coming out of the server.
compress :: Application -> Application
compress = gzip (def { gzipFiles = GzipCompress })

app :: Day -> Pages -> Blogs -> Application
app today ps bs = compress routes
  where
    routes :: Application
    routes req resp = case pathInfo req of
      -- Assets --
      "assets" : rest -> assets (req { pathInfo = rest }) resp
      "webfonts" : rest -> assets (req { pathInfo = rest }) resp
      ".well-known" : rest -> wellKnown (req { pathInfo = rest }) resp
      [ "favicon.ico" ] -> assets req resp
      -- Languageless endpoints --
      [ "drj" ] -> resp $ html drj
      -- Static pages --
      [ lang, "about" ] -> resp $ withLang lang (\l -> html . site l About (Just "About") $ about ps l)
      [ lang, "cv" ] -> resp $ withLang lang (\l -> html . site l CV (Just "CV") $ cv ps l)
      [ lang, "tools", "al-bhed"] -> resp $ withLang lang (\l -> html . site l Tool (Just "Al Bhed Translator") $ alBhed l)
      [ lang, "tools", "twitch"] -> resp $ withLang lang (\l -> html $ site l Tool (Just "Twitch Player") twitch)
      [ _, "tools", "love-letter"] -> resp $ html love
      [ lang, "demo", "game-of-life"] -> resp $ withLang lang (\l -> html . site l Demo (Just "Game of Life") $ gol l)
      [ lang, "demo", "web-effects"] -> resp $ withLang lang (\l -> html . site l Demo (Just "Web Effects Demo") $ webEffects l)
      [ lang, "demo", "seed-effects"] -> resp $ withLang lang (\l -> html . site l Demo (Just "Seed Demo") $ seedEffects l)
      -- All blog posts --
      [ lang, "blog" ] ->
        resp $ withLang lang (\l -> html . (\b -> site l Posts (Just $ blogTitle b) $ blog today bs l (Just b)) $ newest bs l)
      [ lang, "blog", slug ] ->
        resp $ withLang lang (\l -> html . (\mb -> site l Posts (blogTitle <$> mb) $ blog today bs l mb) $ choose bs l slug)
      -- RSS feed --
      [ lang, "rss" ] -> resp $ withLang lang (xml . rss bs)
      -- The language button --
      [ lang ] ->
        resp $ withLang lang (\l -> html . site l Landing Nothing $ landing l)
      -- Index page yields the English landing page --
      [] -> resp . html . site English Landing Nothing $ landing English
      _ -> resp err404

    err404 :: Response
    err404 = responseLBS status404 [("Content-Type", "text/html")]
      . renderBS $ site English Nowhere (Just "404") nowhere

    assets :: Application
    assets = staticApp (defaultFileServerSettings "assets")

    wellKnown :: Application
    wellKnown = staticApp (defaultFileServerSettings ".well-known")

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

-- | Absolute paths to all the @.org@ blog files.
orgFiles :: IO [FilePath]
orgFiles = filter (L.isSuffixOf ".org") <$> (listDirectory "blog" >>= traverse f)
  where
    f fp = makeAbsolute $ "blog" </> fp

-- | Render all ORG files to HTML.
orgs :: NonEmpty FilePath -> IO ([Text], [Blog])
orgs = fmap partitionEithers . traverse g . NEL.toList
  where
    style :: O.OrgStyle
    style = O.defaultStyle
      { O.includeTitle = False
      , O.bulma = True
      , O.highlighting = skylighting }

    g :: FilePath -> IO (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang f
        day <- note ("No date provided for: " <> path) $ orgDate "DATE" ofile
        let !updated = orgDate "UPDATED" ofile
        let !cat = M.lookup "CATEGORY" (O.orgMeta ofile) >>= catFromText
        title <- note ("No title provided for: " <> path) . M.lookup "TITLE" $ O.orgMeta ofile
        Right $ Blog lang (pathSlug f) title day updated cat ofile (O.body style ofile) (O.toc style ofile)

eread :: FilePath -> IO (Either Text Text)
eread path = do
  exists <- doesFileExist path
  if exists
     then Right . T.decodeUtf8With lenientDecode <$> B.readFile path
     else pure . Left $ T.pack path <> " doesn't exist to be read."

pages :: IO (Maybe Pages)
pages = runMaybeT $ Pages
  <$> ab style "org/about-en.org"
  <*> ab style "org/about-jp.org"
  <*> re style "org/cv-en.org" "Colin Woodbury"
  <*> re style "org/cv-jp.org" "ウッドブリ・コリン"
  where
    style :: O.OrgStyle
    style = O.defaultStyle
      { O.includeTitle = False
      , O.tableOfContents = O.TOC 2
      , O.bulma = True
      , O.highlighting  = skylighting }

    ab :: O.OrgStyle -> FilePath -> MaybeT IO (Html ())
    ab s fp = O.body s <$> MaybeT (orgd fp)

    re :: O.OrgStyle -> FilePath -> Text -> MaybeT IO CirVit
    re s fp t = do
      o <- MaybeT $ orgd fp
      pure $ CirVit t (O.body s o) (O.toc s o)

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
  Left err             -> logError err >> exitFailure
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
  today <- utctDay <$> getCurrentTime
  let !prt = fromMaybe 8081 $ p <|> herokuPort
      !eng = mapify ens
      !jap = mapify jps
      !newEs = sortByDate ens
      !newJs = sortByDate jps
      !bls = Blogs (byCats newEs) (byCats newJs) (groupByDate newEs) (groupByDate newJs) eng jap
  logInfo . T.pack $ printf "Blog posts read: %d" (length ens + length jps)
  logInfo . T.pack $ printf "Listening on port %d with %d cores" prt cores
  W.run prt $ app today ps bls

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.reverse . NEL.sortWith blogDate

groupByDate :: NonEmpty Blog -> NonEmpty BlogsByDate
groupByDate = NEL.map f . NEL.groupWith1 fst . NEL.map (gregYear . blogDate &&& id)
  where
    gregYear :: Day -> Integer
    gregYear = (\(y,_,_) -> y) . toGregorian

    f :: NonEmpty (Integer, Blog) -> BlogsByDate
    f a@((y, _) :| _) = BlogsByDate y $ NEL.map snd a

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
