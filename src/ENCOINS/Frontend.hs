{-# LANENCOINS.Website.Head #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ENCOINS.Frontend where

import qualified JS
import Reflex.Dom

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "ENCOINS Wallet"
  el "style" $
    text
      ".wf-force-outline-none[tabindex=\"-1\"]:focus{outline:none;}"
  meta $ "charset" =: "utf-8"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "og:title"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "twitter:title"
  meta $
    "content" =: "width=device-width, initial-scale=1"
      <> "name" =: "viewport"
  stylesheet "css/normalize.css"
  stylesheet "css/webflow.css"
  stylesheet "css/encs-3bb243bf2a8520d5a66d-f872d515808be.webflow.css"
  elAttr
    "link"
    ( "rel" =: "stylesheet" <> "media" =: "all"
        <> "href"
          =: "http://fonts.googleapis.com/css?family=Poppins:400,500,600,700,800|Inter:400,500,700|Mulish:400,500"
    )
    blank
  elAttr
    "link"
    ( "href" =: "images/favicon.png" <> "rel" =: "shortcut icon"
        <> "type" =: "image/x-icon"
    )
    blank
  elAttr
    "link"
    ("href" =: "images/webclip.png" <> "rel" =: "apple-touch-icon")
    blank

  eWebFontLoaded <-
    domEvent Load . fst
      <$> elAttr'
        "script"
        ( "src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"
            <> "type" =: "text/javascript"
        )
        blank
  eCSLLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/CSL.js" <> "type" =: "text/javascript") blank
  eWebpageLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/Webpage.js" <> "type" =: "text/javascript") blank
  eEd25519Loaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/noble-ed25519.js" <> "type" =: "text/javascript") blank

  dWebFontLoaded <- holdDyn False (True <$ eWebFontLoaded)
  dCSLLoaded <- holdDyn False (True <$ eCSLLoaded)
  dWebpageLoaded <- holdDyn False (True <$ eWebpageLoaded)
  dEd25519Loaded <- holdDyn False (True <$ eEd25519Loaded)
  let eScriptsLoaded = ffilter (== True) $ updated $ foldl (zipDynWith (&&)) (pure True) [dWebFontLoaded, dWebpageLoaded, dCSLLoaded, dEd25519Loaded]

  performEvent_ (JS.runHeadScripts <$ eScriptsLoaded)
 where
  meta attr = elAttr "meta" attr blank
  stylesheet href =
    elAttr
      "link"
      ( "href" =: href <> "rel" =: "stylesheet"
          <> "type" =: "text/css"
      )
      blank