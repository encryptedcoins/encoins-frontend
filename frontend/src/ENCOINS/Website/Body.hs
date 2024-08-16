{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Website.Body
    ( bodyWidget
    ) where

import Data.Function (on)
import Data.Text (Text)
import Reflex.Dom

import Common.Events
import ENCOINS.Common.Cache (locale)
import ENCOINS.Common.Widgets.Advanced (waitForScripts)
import ENCOINS.Common.Widgets.JQuery (jQueryWidget)
import ENCOINS.Common.Widgets.Locale (cacheLocaleLanding, decodeLocale)
import ENCOINS.Website.Widgets.Footer (footerWidget)
import ENCOINS.Website.Widgets.LandingPage (landingPage)
import ENCOINS.Website.Widgets.Navbar (navbarWidget)
import I18n.I18n (App)
import I18n.Reflex.I18n (Locale (..), runLocalize)
import JS.Website (loadJSONNoPass)

pageSelect :: (App t m) => (Text, Text) -> m (Event t (Text, Text))
pageSelect (page, idFocus) = case page of
    "Home" -> landingPage idFocus
    _ -> return never

bodyContentWidget :: (App t m) => Locale -> m (Dynamic t Locale)
bodyContentWidget currentLocale = mdo
    divClass "hero" blank

    (eNavbarPageSelected, dLocaleNew) <- navbarWidget dPageFocus currentLocale
    eBodyPageSelected <- dyn (fmap pageSelect dPageFocus) >>= switchHold never
    eFooterPageSelected <- footerWidget

    dPageFocus <-
        holdDyn
            ("Home", "Navbar")
            (leftmost [eNavbarPageSelected, eBodyPageSelected, eFooterPageSelected])
            >>= holdUniqDynBy ((==) `on` fst)

    cacheLocaleLanding dLocaleNew

bodyWidget :: (MonadWidget t m) => m ()
bodyWidget = waitForScripts "loadCacheValue" "js/LandingCommon.js" blank $ mdo
    localeInCache <- decodeLocale <$> loadJSONNoPass locale
    dLocaleNew <- runLocalize dLocale $ bodyContentWidget localeInCache
    dLocale <- holdUniqDyn =<< holdDyn localeInCache (updated dLocaleNew)
    logDyn "bodyWidget" dLocale
    jQueryWidget
