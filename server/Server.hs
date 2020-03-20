{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import           BasePrelude hiding (app, option)
import           Control.Monad.Trans.Maybe (MaybeT(..))
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
import           Data.These (These(..), partitionEithersNE)
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
import           Fosskers.Site.Blog (blog, choose, newest)
import           Fosskers.Site.CV (cv)
import           Lucid
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Applicative hiding (style)
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles (serveDirectoryFileServer)
import           Skylighting hiding (formatHtmlBlock)
import           Skylighting.Format.HTML.Lucid (formatHtmlBlock)
import           System.Directory (doesFileExist, listDirectory, makeAbsolute)
import           System.FilePath ((</>))
import           Text.Megaparsec (errorBundlePretty, parse)

---

newtype Args = Args (Maybe Int)

pArgs :: Parser Args
pArgs = Args
  <$> optional (option auto $ long "port" <> help "Port to listen on, otherwise $PORT")

server :: Pages -> Blogs -> Server API
server ps bs =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about ps l)
  :<|> (\l -> pure . site l CV $ cv ps l)
  :<|> (\l -> pure . site l Posts . blog bs l . Just $ newest bs l)
  :<|> (\l t -> pure . site l Posts . blog bs l $ choose bs l t)
  :<|> pure . rss bs
  :<|> (\l -> pure . site l Posts . blog bs l . Just $ newest bs l)
  :<|> pure (site English Posts . blog bs English . Just $ newest bs English)

rss :: Blogs -> Language -> ByLanguage
rss bs l = ByLanguage $ NEL.sortWith (Down . orgDate . blogRaw) ps
  where
    ps = case l of
      English  -> engSorted bs
      Japanese -> japSorted bs

app :: Pages -> Blogs -> Application
app ps bs = gzip (def { gzipFiles = GzipCompress })
  . serve (Proxy :: Proxy API)
  $ server ps bs

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

-- | Abosolute paths to all the @.org@ blog files.
orgFiles :: IO [FilePath]
orgFiles = filter (L.isSuffixOf ".org") <$> (listDirectory "blog" >>= traverse f)
  where
    f fp = makeAbsolute $ "blog" </> fp

-- | Render all ORG files to HTML.
orgs :: NonEmpty FilePath -> IO (These (NonEmpty Text) (NonEmpty Blog))
orgs = fmap partitionEithersNE . traverse g
  where
    style = O.OrgStyle True (Just $ O.TOC "Contents" 2) True skylighting (Just ' ')

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
    astyle = O.OrgStyle False Nothing False skylighting (Just ' ')
    cstyle = O.OrgStyle True (Just $ O.TOC "Index" 2) True skylighting (Just ' ')
    jstyle = O.OrgStyle True (Just $ O.TOC "目次" 2) True skylighting (Just ' ')

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
setup args = pages >>= \case
  Nothing -> logError "Couldn't read static pages."
  Just ps -> fmap NEL.nonEmpty orgFiles >>= \case
    Nothing -> logError "No .org files were found." >> exitFailure
    Just fs -> orgs fs >>= \case
      This errs -> traverse_ logWarn errs >> exitFailure
      That bs -> case splitLs bs of
        Nothing -> logError "Missing posts from Japanese or English" >> exitFailure
        Just r  -> uncurry (work args ps) r
      These errs bs -> do
        traverse_ logWarn errs
        maybe exitFailure (uncurry $ work args ps) $ splitLs bs

splitLs :: NonEmpty Blog -> Maybe (NonEmpty Blog, NonEmpty Blog)
splitLs = bitraverse NEL.nonEmpty NEL.nonEmpty . NEL.partition (\b -> blogLang b == English)

work :: Args -> Pages -> NonEmpty Blog -> NonEmpty Blog -> IO ()
work (Args p) ps ens jps = do
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  cores <- getNumCapabilities
  let !prt = fromMaybe 8081 $ p <|> herokuPort
      !eng = mapify ens
      !jap = mapify jps
      !bls = Blogs (sortByDate ens) (sortByDate jps) eng jap
  logInfo . T.pack $ printf "Blog posts read: %d" (length ens + length jps)
  logInfo . T.pack $ printf "Listening on port %d with %d cores" prt cores
  W.run prt $ app ps bls

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.reverse . NEL.sortWith (orgDate . blogRaw)

mapify :: NonEmpty Blog -> Map Text Blog
mapify = M.fromList . NEL.toList . NEL.map (blogSlug &&& id)

logError :: Text -> IO ()
logError t = T.putStrLn $ "[error] " <> t

logWarn :: Text -> IO ()
logWarn t = T.putStrLn $ "[warn] " <> t

logInfo :: Text -> IO ()
logInfo t = T.putStrLn $ "[info] " <> t
