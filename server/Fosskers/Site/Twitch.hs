module Fosskers.Site.Twitch ( twitch ) where

import BasePrelude
import Lucid
import Lucid.Base

---

twitch :: Html ()
twitch = div_ [class_ "grid-main"] $ div_ [class_ "twitch"] $ do
  -- Input
  div_ [classes_ ["field", "has-addons"]] $ do
    div_ [class_ "control"] $
      input_ [id_ "twitch-input", classes_ ["input", "is-rounded"], type_ "text", placeholder_ "Twitch Channel"]
    div_ [class_ "control"] $
      button_ [id_ "twitch-watch", classes_ ["button", "is-success", "is-rounded"]] $ do
        span_ "Watch"
        span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-twitch"]] ""

  -- Friend buttons
  div_ [classes_ ["buttons", "has-addons"]] $ do
    button_ [id_ "twitch-dan", classes_ ["button", "is-primary", "is-rounded"]] $ do
      span_ "choccy_soup"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-drum"]] ""
    button_ [id_ "twitch-john", classes_ ["button", "is-danger", "is-rounded"]] $ do
      span_ "ace_deuce"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-gamepad"]] ""
    button_ [id_ "twitch-mia", classes_ ["button", "is-link", "is-rounded"]] $ do
      span_ "miametzmusic"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-music"]] ""

  -- Player
  div_ [class_ "twitch-player"] $ do
    iframe_ [ id_ "twitch-embedded-player"
            , src_ "https://player.twitch.tv/?channel=choccy_soup&parent=www.fosskers.ca&autoplay=false"
            , makeAttribute "frameborder" "0"
            , makeAttribute "scrolling" "no"
            , makeAttribute "allowfullscreen" "true" ] ""

  -- Activate the code
  wasm

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/twitch.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
