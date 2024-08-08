module ENCOINS.Common.Widgets.Locale (localeWidget) where

import Reflex.Dom

import Common.Events
import I18n.I18n (App)
import I18n.Reflex.I18n (Locale (..))

localeWidget :: (App t m) => m (Dynamic t Locale)
localeWidget = do
    let conf = def{_dropdownConfig_attributes = constDyn $ "class" =: "common-Nav_Dropdown"}
    res <-
        dropdown
            Locale_EN
            (constDyn ((Locale_EN :: Locale) =: "EN" <> Locale_RU =: "RU"))
            conf
    pure $ _dropdown_value res
