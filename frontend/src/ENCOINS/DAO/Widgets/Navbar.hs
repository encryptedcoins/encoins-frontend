module ENCOINS.DAO.Widgets.Navbar
  (
    navbarWidget
  , Dao (..)
  ) where

import           Data.Text                     (Text, take, takeEnd)
import           Prelude                       hiding (take)
import           Reflex.Dom

import           Backend.Wallet                (NetworkConfig (dao),
                                                NetworkId (..), Wallet (..),
                                                WalletName (..), networkConfig)
import           ENCOINS.Common.Widgets.Basic  (btn, btnWithBlock, logo)
import           ENCOINS.Common.Widgets.Wallet (walletIcon)


data Dao = Connect | Delegate
  deriving (Eq, Show)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _ _    _ _ -> "CONNECT"
  Wallet _    _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Bool
  -> m (Event t Dao)
navbarWidget w dIsBlocked = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "h4"
              $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;")
              $ text "DAO"
            divClass "h4"
              $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;")
              $ text currentNetworkDao
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                eConnect <- divClass "menu-item-button-left" $
                    btn "button-switching flex-center" "" $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                eDelegate <- divClass "menu-item-button-left" $ do
                    btnWithBlock
                      "button-switching flex-center"
                      ""
                      dIsBlocked
                      (text "DELEGATE")
                pure $ leftmost [Connect <$ eConnect, Delegate <$ eDelegate]

currentNetworkDao :: Text
currentNetworkDao = case dao networkConfig of
  Mainnet -> "Mainnet"
  Testnet -> "Testnet"
