module ENCOINS.DAO.Widgets.Navbar
    ( Dao (..)
    , navbarWidget
    ) where

import Data.Text (Text)
import Reflex.Dom

import Backend.Wallet (Wallet (..))
import Common.Reflex.Dom.Extra (textLocale)
import Config.Config (NetworkConfig (dao), NetworkId (..), networkConfig)
import ENCOINS.Common.Widgets.Basic (btnWithBlock, logo)
import ENCOINS.Common.Widgets.Connect (connectWidget)
import ENCOINS.Common.Widgets.Locale (localeWidget)
import ENCOINS.Common.Widgets.MoreMenu (NavMoreMenuClass (..), viewMoreMenu)
import qualified I18n.Dao as I18n
import I18n.I18n (App)
import I18n.Reflex.I18n (Locale)

data Dao = Connect | Delegate | MoreMenu
    deriving (Eq, Show)

navbarWidget ::
    (App t m) =>
    Dynamic t Wallet
    -> Dynamic t Bool
    -> Dynamic t Bool
    -> Locale
    -> m (Event t Dao, Dynamic t Locale)
navbarWidget w dIsBlocked dIsBlockedConnect currentLocale = do
    elAttr
        "div"
        ( "data-animation" =: "default"
            <> "data-collapse" =: "none"
            <> "data-duration" =: "400"
            <> "id" =: "Navbar"
            <> "data-easing" =: "ease"
            <> "data-easing2" =: "ease"
            <> "role" =: "banner"
            <> "class" =: "navbar w-nav"
        )
        $ divClass "navbar-container w-container"
        $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
                logo
                divClass "h3" $ text "ENCOINS"
            divClass "h4" $
                elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;") $
                    text "DAO"
            divClass "h4" $
                elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;") $
                    text currentNetworkDao
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                eConnect <- connectWidget w dIsBlockedConnect
                eDelegate <- divClass "menu-item-button-left" $ do
                    btnWithBlock
                        "button-switching flex-center"
                        ""
                        dIsBlocked
                        (textLocale I18n.Delegate)
                dLocale <- localeWidget "common-Nav_Dropdown" currentLocale
                eMore <-
                    viewMoreMenu
                        (NavMoreMenuClass "common-Nav_Container_MoreMenu" "common-Nav_MoreMenu")
                pure
                    ( leftmost [Connect <$ eConnect, Delegate <$ eDelegate, MoreMenu <$ eMore]
                    , dLocale
                    )

currentNetworkDao :: Text
currentNetworkDao = case dao networkConfig of
    Mainnet -> "Mainnet"
    Testnet -> "Testnet"
