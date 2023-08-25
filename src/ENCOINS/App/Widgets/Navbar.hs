module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text                        (Text, take, takeEnd)
import           Prelude                          hiding (take)
import           Reflex.Dom

import           Backend.Wallet                   (Wallet (..), WalletName (..), walletIcon)
import           ENCOINS.Common.Widgets.Advanced  (logo)
import           ENCOINS.Common.Widgets.Basic     (btn)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw )

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _ _    _ _ -> "CONNECT"
  Wallet _    _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Maybe PasswordRaw
  -> m (Event t (), Event t ())
navbarWidget w mPass = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "h4" $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;") $ text "Testnet Preprod"
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                elLocker <- lockerWidget mPass
                eConnect <- divClass "menu-item-button-left" $
                    btn "button-switching flex-center" "" $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                return (domEvent Click elLocker, eConnect)

lockerWidget :: MonadWidget  t m
  => Maybe PasswordRaw
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerWidget mPass = do
  let (iconClass, popupClass, popupText) = case mPass of
        Nothing -> ("menu-item-unlocked", "menu-item-unlocked-text", "doesn't protected")
        Just _ -> ("menu-item-locked", "menu-item-locked-text", "protected")
  fst <$> lockerDiv iconClass popupClass popupText

lockerDiv :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> m (Element EventResult (DomBuilderSpace m) t, ())
lockerDiv iconClass popupClass popupText
  = elClass' "div" ("menu-item menu-item-button-left" <> iconClass <> "w-inline-block")
  $ divClass ("menu-item menu-item-button-left" <> popupClass)
  $ el "p" $ text $ "Cache" <> popupText