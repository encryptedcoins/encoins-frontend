module ENCOINS.DAO.Widgets.Navbar
    ( Dao (..)
    , navbarWidget
    ) where

import Data.Text (Text)
import Reflex.Dom

import Backend.Wallet (Wallet (..))
import Config.Config (NetworkConfig (dao), NetworkId (..), networkConfig)
import ENCOINS.Common.Widgets.Basic (btnWithBlock, logo)
import ENCOINS.Common.Widgets.Connect (connectWidget)
import ENCOINS.Common.Widgets.MoreMenu (NavMoreMenuClass (..), moreMenuWidget)

data Dao = Connect | Delegate | MoreMenu
    deriving (Eq, Show)

navbarWidget ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t Bool
    -> Dynamic t Bool
    -> m (Event t Dao)
navbarWidget w dIsBlocked dIsBlockedConnect = do
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
                        (text "DELEGATE")
                eMore <-
                    moreMenuWidget
                        (NavMoreMenuClass "common-Nav_Container_MoreMenu" "common-Nav_MoreMenu")
                pure $ leftmost [Connect <$ eConnect, Delegate <$ eDelegate, MoreMenu <$ eMore]

currentNetworkDao :: Text
currentNetworkDao = case dao networkConfig of
    Mainnet -> "Mainnet"
    Testnet -> "Testnet"
