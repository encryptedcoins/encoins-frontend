module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Text                          (Text, take, takeEnd)
import           Prelude                            hiding (take)
import           Reflex.Dom

import           Backend.Wallet                     (Wallet (..),
                                                     WalletName (..),
                                                     currentNetworkApp)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw)
import           ENCOINS.Common.Widgets.Basic       (btnWithBlock, logo,
                                                     space)
import           ENCOINS.Common.Widgets.Wallet      (walletIcon)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _ _    _ _ -> "CONNECT"
  Wallet _    _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Bool
  -> Maybe PasswordRaw
  -> m (Event t (), Event t ())
navbarWidget w dIsBlockConnect mPass = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "h4"
              $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;")
              $ text currentNetworkApp
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                elLocker <- lockerWidget mPass
                eConnect <- divClass "menu-item-button-left" $
                    btnWithBlock "button-switching flex-center" "" dIsBlockConnect $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                return (domEvent Click elLocker, eConnect)

lockerWidget :: MonadWidget  t m
  => Maybe PasswordRaw
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerWidget mPass = do
  let (iconClass, popupText) = case mPass of
        Nothing -> ("app-Nav_Locker-open", "isn't protected")
        Just _  -> ("app-Nav_Locker-close", "is protected")
  lockerDiv iconClass popupText

lockerDiv :: MonadWidget t m
  => Text
  -> Text
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerDiv iconClass popupText
  = fst <$> elClass' "div"
    ("menu-item app-Nav_LockerContainer" <> space <> iconClass)
    (divClass "app-Nav_CachePopup"
      $ el "p" $ text $ "Cache" <> space <> popupText)
