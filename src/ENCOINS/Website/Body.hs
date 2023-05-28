module ENCOINS.Website.Body (bodyWidget) where

import           Data.Functor                        (($>))
import           Data.Text                           (Text)
import           Reflex.Dom

import           ENCOINS.Website.Widgets.Footer      (footerWidget)
import           ENCOINS.Website.Widgets.ISPO        (ispoPage)
import           ENCOINS.Website.Widgets.LandingPage (landingPage)
import           ENCOINS.Website.Widgets.Navbar      (navbarWidget)

pageSelect :: MonadWidget t m => (Text, Text) -> m (Event t (Text, Text))
pageSelect (page, idFocus) = case page of
  "Home" -> landingPage idFocus
  "ISPO" -> never <$ ispoPage
  _      -> return never
    
bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  divClass "hero" blank

  eNavbarPageSelected <- navbarWidget dPageFocus
  eBodyPageSelected   <- dyn (fmap pageSelect dPageFocus) >>= switchHold never
  eFooterPageSelected <- footerWidget

  dPageFocus <- holdDyn ("Home", "Navbar") (leftmost [eNavbarPageSelected, eBodyPageSelected, eFooterPageSelected])
    >>= holdUniqDynBy (\a b -> fst a == fst b)

  blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  bodyContentWidget

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e