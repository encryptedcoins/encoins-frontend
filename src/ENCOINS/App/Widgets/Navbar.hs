module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text                 (Text, take, takeEnd)
import           Prelude                   hiding (take)
import           Reflex.Dom

import           Backend.Wallet            (Wallet (..), WalletName (..), walletIcon)
import           ENCOINS.App.Widgets.Basic (btnApp)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _    _ _ -> "CONNECT"
  Wallet _    addr _ _ -> take 4 addr <> "..." <> takeEnd 4 addr

navbarWidget :: MonadWidget t m => Dynamic t Wallet -> m (Event t ())
navbarWidget w = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "medium" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "div-navbar" $
        divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "index.html" <> "class" =: "brand w-nav-brand") $
                elAttr "img" ("src" =: "images/logo.svg" <> "loading" =: "lazy" <> "alt" =: "" <> "class" =: "logo") blank
            divClass "h3" $ text "ENCOINS"
            divClass "menu-div-empty" blank
            elAttr "a" ("href" =: "#" <> "class" =: "menu-item menu-item-settings w-inline-block") blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                divClass "menu-item-button-left" $ do
                    _ <- btnApp "button-switching" $ dynText "RELAYER"
                    blank
                divClass "menu-item-button-left" $
                    btnApp "button-switching" $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w