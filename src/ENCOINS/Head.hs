module ENCOINS.Head (headWidget) where

import           Reflex.Dom

import qualified JS

headWidget :: MonadWidget t m => m ()
headWidget = do
  meta $ "charset" =: "utf-8"
  el "title" $ text "ENCOINS"
  meta $ "content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport"
  meta $ "content" =: "Webflow" <> "name" =: "generator"
  stylesheet "css/normalize.css"
  stylesheet "css/webflow.css"
  stylesheet "css/encoins.webflow.css"
  stylesheet "css/encoins.webflow-extra.css"
  elAttr "link" ("href" =: "images/favicon.png" <> "rel" =: "shortcut icon" <> "type" =: "image/x-icon") blank
  elAttr "link" ("href" =: "images/webclip.png" <> "rel" =: "apple-touch-icon") blank

  eWebFontLoaded <- domEvent Load . fst <$>
    elAttr' "script" ("src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js" <> "type" =: "text/javascript") blank
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
  stylesheet href = elAttr "link" ("href" =: href <> "rel" =: "stylesheet" <> "type" =: "text/css") blank