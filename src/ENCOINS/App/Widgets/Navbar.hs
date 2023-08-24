module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text                        (Text, take, takeEnd)
import           Prelude                          hiding (take)
import           Reflex.Dom

import           Backend.Wallet                   (Wallet (..), WalletName (..), walletIcon)
import           ENCOINS.Common.Widgets.Advanced  (logo)
import           ENCOINS.Common.Widgets.Basic     (btn)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _ _    _ _ -> "CONNECT"
  Wallet _    _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m => Dynamic t Wallet -> m (Event t (), Event t ())
navbarWidget w = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "h4" $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;") $ text "Testnet Preprod"
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                (elSettings,_) <- elClass' "div"
                    "menu-item menu-item-button-left menu-item-settings w-inline-block" $
                      divClass "menu-item menu-item-button-left menu-item-settings-text" $
                        el "p" $ text "Protect cache"
                eConnect <- divClass "menu-item-button-left" $
                    btn "button-switching flex-center" "" $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                return (domEvent Click elSettings, eConnect)
