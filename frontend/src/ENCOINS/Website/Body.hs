{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Website.Body (bodyWidget) where

import           Data.Function                       (on)
import           Data.Text                           (Text)
import           Reflex.Dom

import           ENCOINS.Common.Widgets.JQuery       (jQueryWidget)
import           ENCOINS.Website.Widgets.Footer      (footerWidget)
import           ENCOINS.Website.Widgets.ISPOPage    (ispoPage)
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
    >>= holdUniqDynBy ((==) `on` fst)

  blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  bodyContentWidget
  jQueryWidget
