module ENCOINS.App.Widgets.WelcomeWindow where

import           Data.Aeson                      (ToJSON, FromJSON, encode, decode)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Text                       (Text)
import           Data.Text.Encoding              (encodeUtf8, decodeUtf8)
import           GHCJS.DOM.Types                 (MonadDOM)
import           GHCJS.DOM                       (currentWindowUnchecked)
import           GHCJS.DOM.Storage               (getItem, setItem)
import           GHCJS.DOM.Window                (getLocalStorage)
import           Reflex.Dom

import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (setElementStyle)

data WelcomeItem = WelcomeItem
  { elemId :: Text
  , message :: Text
  -- , styles :: Text
  }

welcomeWallet :: [WelcomeItem]
welcomeWallet =
  [ WelcomeItem "welcome-tabs"
    "Welcome to ENCOINS! You can select the mode of operation from this menu."
  , WelcomeItem "welcome-wallet-coins"
    "In this column, you will see the coins in your wallet. \
     \Clicking on a particular coin will reveal its asset fingerprint. \
     \To select coins to burn in the current transaction, check the boxes on the left."
  , WelcomeItem "welcome-coins-mint"
    "Here, you can mint new coins by entering their ADA value and hitting \
    \the \"+\" button. You can remove a coin from the current transaction by \
    \hitting the \"x\" button."
  , WelcomeItem "welcome-tx-balance"
    "Here you can see the net ADA balance of the current transaction and protocol fees."
  , WelcomeItem "welcome-send-req"
    "Once you finished building the transaction, press this button to execute it."
  , WelcomeItem "welcome-import-export"
    "All known coins are stored locally on your device. Here you can import \
    \the coins that are new to this device. You can also export your coins \
    \for backup or to use on another device." ]

welcomeTransfer :: [WelcomeItem]
welcomeTransfer =
  [ WelcomeItem "welcome-coins-transfer"
    "You are now in the transfer mode! To select the coins to transfer, \
    \check the boxes on the left."
  , WelcomeItem "welcome-transfer-btns"
    "You can send coins to another user or to the ENCOINS Ledger for use in the Ledger Mode." ]

welcomeTutorial :: MonadWidget t m => [WelcomeItem] -> Event t () -> m (Event t ())
welcomeTutorial [] eOpen = pure eOpen
welcomeTutorial (wi:wis) eOpen = do
  eWiNext <- welcomeItemWidget wi eOpen
  eOpenNext <- delay 0.3 eWiNext
  welcomeTutorial wis eOpenNext

welcomeItemWidget :: MonadWidget t m => WelcomeItem -> Event t () -> m (Event t ())
welcomeItemWidget WelcomeItem{..} eOpen = mdo
  performEvent_ (setElementStyle elemId "z-index" "2000" <$ eOpen)
  performEvent_ (setElementStyle elemId "position" "relative" <$ eOpen)
  performEvent_ (setElementStyle elemId "pointer-events" "none" <$ eOpen)
  eClose <- dialogWindow eOpen eClose "max-width: 700px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px; top: 0px; position: absolute;" $ do
    text message
    btn "button-switching inverted flex-center" "" $ text "Ok"
  performEvent_ (setElementStyle elemId "z-index" "" <$ eClose)
  performEvent_ (setElementStyle elemId "position" "" <$ eClose)
  performEvent_ (setElementStyle elemId "pointer-events" "" <$ eClose)
  return eClose

welcomeWindowWalletStorageKey :: Text
welcomeWindowWalletStorageKey = "encoins-welcome-window-seen-wallet"

welcomeWindowTransferStorageKey :: Text
welcomeWindowTransferStorageKey = "encoins-welcome-window-seen-transfer"

loadJsonFromStorage :: (MonadDOM m, FromJSON a) => Text -> m (Maybe a)
loadJsonFromStorage elId = do
  lc <- currentWindowUnchecked >>= getLocalStorage
  (>>= decode . fromStrict . encodeUtf8) <$> getItem lc elId

saveJsonToStorage :: (MonadDOM m, ToJSON a) => Text -> a -> m ()
saveJsonToStorage elId val = do
  lc <- currentWindowUnchecked >>= getLocalStorage
  setItem lc elId . decodeUtf8 . toStrict . encode $ val

welcomeWindow :: MonadWidget t m => Text -> [WelcomeItem] -> m ()
welcomeWindow key items = do
    isSeen <- loadJsonFromStorage key
    if isSeen == Just True
        then blank
        else do
            ePb <- getPostBuild
            eWelcome <- delay 0.5 ePb
            eWelcomeClose <- welcomeTutorial items eWelcome
            performEvent_ (saveJsonToStorage key True <$
              eWelcomeClose)
