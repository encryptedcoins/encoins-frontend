module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text              (Text)
import           Reflex.Dom

navbarWidget :: MonadWidget t m => Dynamic t (Text, Text) -> m (Event t (Text, Text))
navbarWidget _ = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "medium" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "div-navbar" $
        divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "index.html" <> "class" =: "brand w-nav-brand") $
                elAttr "img" ("src" =: "images/logo.svg" <> "loading" =: "lazy" <> "alt" =: "" <> "class" =: "logo") blank
            divClass "h3" $ text "ENCOINS"
            divClass "menu-div-empty" blank
            elAttr "a" ("href" =: "#" <> "class" =: "menu-item menu-item-settings w-inline-block") blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ mdo
                divClass "menu-item-button-left" $
                    elAttr "a" ("href" =: "#" <> "class" =: "app-button button-not-selected w-button") $ text "RELAYER"
                divClass "menu-item-button-left" $
                    elAttr "a" ("href" =: "#" <> "class" =: "app-button button-not-selected w-button") $ text "CONNECT"
            return never