module Fosskers.Site.Twitch ( twitch ) where

import BasePrelude
import Lucid
import Lucid.Base (makeAttribute)

---

twitch :: Html ()
twitch = div_ [class_ "grid-main"] $ div_ [class_ "twitch"] $ do
  div_ [classes_ ["field", "has-addons"]] $ do
    div_ [class_ "control"] $
      input_ [classes_ ["input", "is-rounded"], type_ "text", placeholder_ "Twitch Channel"]
    div_ [class_ "control"] $
      a_ [classes_ ["button", "is-success", "is-rounded"]] $ do
        span_ "Watch"
        span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-twitch"]] ""
  div_ [classes_ ["buttons", "has-addons"]] $ do
    a_ [classes_ ["button", "is-primary", "is-rounded"]] $ do
      span_ "choccy_soup"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-drum"]] ""
    a_ [classes_ ["button", "is-danger", "is-rounded"]] $ do
      span_ "ace_deuce"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-gamepad"]] ""
    a_ [classes_ ["button", "is-link", "is-rounded"]] $ do
      span_ "miametzmusic"
      span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-music"]] ""
  div_ [class_ "twitch-player"] $ do
    iframe_ [ src_ "https://player.twitch.tv/?channel=choccy_soup&parent=localhost&autoplay=false"
            , makeAttribute "frameborder" "0"
            , makeAttribute "scrolling" "no"
            , makeAttribute "allowfullscreen" "true" ] ""
