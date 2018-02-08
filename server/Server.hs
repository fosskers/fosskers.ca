{-# LANGUAGE DataKinds, TypeOperators, Rank2Types #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Common
import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Char (isAlpha)
import           Data.Proxy
import qualified Data.Text as T
import           Filesystem.Path (basename)
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Org (parseEng, parseJap)
import           Protolude hiding (FilePath)
import           Servant.Server
import           Shelly hiding (path)

---

newtype Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

newtype Env = Env { posts :: [Blog] }

server :: Env -> Server API
server env = pure (posts env)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

-- | A mapping of word frequencies.
freq :: Text -> [(Text, Int)]
freq = map ((maybe "死毒殺悪厄魔" identity . head) &&& length) . group . sort . map T.toLower . filter p . T.words . T.map f
  where f c = bool ' ' c $ isAlpha c
        p (T.length -> l) = l > 2 && l < 13

org :: Text -> Sh ()
org f = run_ "emacs" [f, "--batch", "-f", "org-html-export-to-html", "--kill"]

-- | The expected filepath of the Japanese version of some blog post,
-- given its full English filepath.
japPath :: Text -> Text
japPath path = T.dropEnd 4 path <> "-jap.org"

-- | Render all ORG files to HTML, also yielding word frequencies for each file.
--
-- Can assume:
--   - Every English article has a Japanese analogue
--   - The true title always appears on the first line of the file
--   - The original (ballpark) date of writing is on the second line of the "base" file
orgs :: Sh ([Text], [Blog])
orgs = do
  cd "blog"
  files <- filter (T.isSuffixOf ".org") <$> lsT "."
  traverse_ org files
  vs <- traverse g $ filter (not . T.isSuffixOf "-jap.org") files
  pure $ partitionEithers vs
  where g :: Text -> Sh (Either Text Blog)
        g f = do
          let engPath = fromText f
          engContent <- eread engPath
          japContent <- eread . fromText $ japPath f
          pure . join $ h (toTextIgnore $ basename engPath) <$> engContent <*> japContent

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
  (errs, ps) <- shelly orgs
  traverse_ putText errs
  let prt = maybe 8081 identity p
  putText $ "Listening on port " <> show prt
  W.run prt . app $ Env ps
