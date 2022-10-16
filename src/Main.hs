module Main where

import           Reflex.Dom

import           AppWidget            (appWidget)
import qualified JS

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "ENCOINS Wallet"
  el "style" $ text
    ".wf-force-outline-none[tabindex=\"-1\"]:focus{outline:none;}"
  meta $ "charset" =: "utf-8"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "og:title"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "twitter:title"
  meta $ "content" =: "width=device-width, initial-scale=1" <>
    "name" =: "viewport"
  stylesheet "css/normalize.css"
  stylesheet "css/webflow.css"
  stylesheet "css/encs-3bb243bf2a8520d5a66d-f872d515808be.webflow.css"
  elAttr "link" ("rel" =: "stylesheet" <> "media" =: "all" <>
    "href" =: "http://fonts.googleapis.com/css?family=Droid+Serif:400,\
      \400italic,700,700italic%7CCorben:regular%7CFenix:regular") blank
  elAttr "link" ("href" =: "images/favicon.png" <> "rel" =: "shortcut icon" <>
    "type" =: "image/x-icon") blank
  elAttr "link" ("href" =: "images/webclip.png" <> "rel" =: "apple-touch-icon")
    blank

  eWebFontLoaded <- domEvent Load . fst <$> elAttr' "script"
    ("src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"
      <> "type" =: "text/javascript") blank
  eEncoinsLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/ENCOINS.js" <> "type" =: "text/javascript") blank
  eCSLLoaded  <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/CSL.js" <> "type" =: "text/javascript") blank

  dWebFontLoaded  <- holdDyn False (True <$ eWebFontLoaded)
  dEncoinsLoaded   <- holdDyn False (True <$ eEncoinsLoaded)
  dCSLLoaded   <- holdDyn False (True <$ eCSLLoaded)
  let eScriptsLoaded = ffilter (== True) $ updated $ foldl (zipDynWith (&&)) (pure True) [dWebFontLoaded, dEncoinsLoaded, dCSLLoaded]

  performEvent_ (JS.runHeadScripts <$ eScriptsLoaded)
  where
    meta attr = elAttr "meta" attr blank
    stylesheet href = elAttr "link" ("href" =: href <> "rel" =: "stylesheet"
      <> "type" =: "text/css") blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  navbarWidget
  logoWidget
  appWidget
  footerWidget

navbarWidget :: MonadWidget t m => m ()
navbarWidget = blank

logoWidget :: MonadWidget t m => m ()
logoWidget = blank

footerWidget :: MonadWidget t m => m ()
footerWidget = blank