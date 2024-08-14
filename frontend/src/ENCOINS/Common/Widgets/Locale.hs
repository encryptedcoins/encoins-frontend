module ENCOINS.Common.Widgets.Locale where

import Data.Aeson (decodeStrict)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Reflex.Dom

import Common.Events
import ENCOINS.Common.Cache
    ( loadAppData
    , locale
    , saveAppData
    )
import I18n.I18n (App)
import I18n.Reflex.I18n (Locale (..))

localeWidget :: (App t m) => Locale -> m (Dynamic t Locale)
localeWidget currentLocale = do
    let conf = def{_dropdownConfig_attributes = constDyn $ "class" =: "common-Nav_Dropdown"}
    res <-
        dropdown
            currentLocale
            (constDyn ((Locale_EN :: Locale) =: "EN" <> Locale_RU =: "RU"))
            conf
    pure $ _dropdown_value res

cacheLocale ::
    (MonadWidget t m) =>
    Dynamic t Locale
    -> m (Dynamic t Locale)
cacheLocale dLocaleNew = do
    eLocaleSaved <- saveAppData Nothing locale $ updated dLocaleNew
    dLocale <-
        loadAppData
            Nothing
            locale
            "cacheLocale-load-locale-key"
            eLocaleSaved
            id
            Locale_EN
    pure dLocale

decodeLocale :: Text -> Locale
decodeLocale =
    fromMaybe Locale_EN . (decodeStrict :: ByteString -> Maybe Locale) . encodeUtf8
