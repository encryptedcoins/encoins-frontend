module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text                        (Text, take, takeEnd)
import           Prelude                          hiding (take)
import           Reflex.Dom

import           Backend.Wallet                   (Wallet (..), WalletName (..), walletIcon)
import           ENCOINS.Common.Widgets.Advanced  (logo)
import           ENCOINS.Common.Widgets.Basic     (btn)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _    _ _ -> "CONNECT"
  Wallet _    addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m => Dynamic t Wallet -> m (Event t ())
navbarWidget w = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "index.html" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "menu-div-empty" blank
            -- elAttr "a" ("href" =: "#" <> "class" =: "menu-item menu-item-settings w-inline-block") blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                -- divClass "menu-item-button-left" $ do
                --     _ <- btnApp "button-switching" $ dynText "RELAYER"
                --     blank
                divClass "menu-item-button-left" $
                    btn "button-switching flex-center" "" $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w