module ENCOINS.Website.Head (headWidget) where

import           Reflex.Dom

import qualified JS.Website as JS

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
  stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/css/all.min.css"

  elAttr "link" ("href" =: "images/favicon.png" <> "rel" =: "shortcut icon" <> "type" =: "image/x-icon") blank
  elAttr "link" ("href" =: "images/webclip.png" <> "rel" =: "apple-touch-icon") blank

  eWebFontLoaded <- domEvent Load . fst <$>
    elAttr' "script" ("src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js" <> "type" =: "text/javascript") blank
  eCSLLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/CSL.js" <> "type" =: "text/javascript") blank
  eLucidLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/Lucid.js" <> "type" =: "text/javascript") blank
  eWebpageLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/Webpage.js" <> "type" =: "text/javascript") blank
  eEd25519Loaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/noble-ed25519.js" <> "type" =: "text/javascript") blank
  eCIP14Loaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/cip14.js" <> "type" =: "text/javascript") blank
  eCrypoJSLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/crypto-js.min.js" <> "type" =: "text/javascript") blank

  dWebFontLoaded <- holdDyn False (True <$ eWebFontLoaded)
  dCSLLoaded <- holdDyn False (True <$ eCSLLoaded)
  dLucidLoaded <- holdDyn False (True <$ eLucidLoaded)
  dWebpageLoaded <- holdDyn False (True <$ eWebpageLoaded)
  dEd25519Loaded <- holdDyn False (True <$ eEd25519Loaded)
  dCIP14Loaded <- holdDyn False (True <$ eCIP14Loaded)
  dCrypoJSLoaded <- holdDyn False (True <$ eCrypoJSLoaded)
  let eScriptsLoaded = ffilter (== True) $ updated $ foldl (zipDynWith (&&)) (pure True)
        [dWebFontLoaded, dWebpageLoaded, dCSLLoaded, dLucidLoaded, dEd25519Loaded, dCIP14Loaded, dCrypoJSLoaded]

  performEvent_ (JS.runHeadScripts <$ eScriptsLoaded)
 where
  meta attr = elAttr "meta" attr blank
  stylesheet href = elAttr "link" ("href" =: href <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
