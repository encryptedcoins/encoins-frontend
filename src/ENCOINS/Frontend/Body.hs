module ENCOINS.Frontend.Body where

import           Data.Functor                (($>))
import           Data.Text                   (Text)
import           Reflex.Dom

import           ENCOINS.Website.Widgets

pageSelect :: MonadWidget t m => Text -> m (Event t Text)
pageSelect page = case page of
  "Home" -> landingPage
  "ISPO" -> never <$ ispoPage
  _      -> return never
    
bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  divClass "hero" blank

  eNavbarPageSelected <- navbarWidget dPage
  eBodyPageSelected   <- dyn (fmap pageSelect dPage) >>= switchHold never
  eFooterPageSelected <- footerWidget

  dPage <- holdDyn "Home" (leftmost [eNavbarPageSelected, eBodyPageSelected, eFooterPageSelected]) >>= holdUniqDyn

  blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  bodyContentWidget

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e