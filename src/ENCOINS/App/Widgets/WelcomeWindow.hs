module ENCOINS.App.Widgets.WelcomeWindow where

import           Control.Monad                   (when)
import           Data.Text                       (Text)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic       (loadJsonFromStorage, saveJsonToStorage)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (setElementStyle)

data WelcomeItem = WelcomeItem
  { elemId :: Text
  , message :: Text
  , border :: Bool
  }

welcomeWallet :: [WelcomeItem]
welcomeWallet =
  [ WelcomeItem "welcome-tabs"
    "Welcome to ENCOINS! You can select the mode of operation from this menu."
    False
  , WelcomeItem "welcome-wallet-coins"
    "In this column, you will see the coins from your wallet. \
     \Clicking on a particular coin will reveal its full name and asset fingerprint. \
     \To select coins to burn in the current transaction, check the boxes on the left. \
     \To copy the minting key, click on the key icon."
    True
  , WelcomeItem "welcome-coins-mint"
    "Here, you can add new coins to mint by entering their ADA value and clicking \
    \the \"+\" button (or by pressing \"Enter\" on the keyboard). You can remove a coin from the current transaction by \
    \clicking the \"x\" button."
    True
  , WelcomeItem "welcome-tx-balance"
    "Here, you can see the net ADA balance of the current transaction and protocol fees. A positive balance means you are withdrawing ADA from the protocol."
    False
  , WelcomeItem "welcome-send-req"
    "Once you finished building the transaction, you can press this button to execute it."
    True
  , WelcomeItem "welcome-import-export"
    "All known coins are stored locally on your device. Here, you can import \
    \ the coins that are new to this device. You can also export your \
    \ coins for backup or use on another device."
    False ]

welcomeTransfer :: [WelcomeItem]
welcomeTransfer =
  [ WelcomeItem "welcome-coins-transfer"
    "You are now in the transfer mode! To select the coins to transfer, \
    \check the boxes on the left."
    True
  , WelcomeItem "welcome-transfer-btns"
    "You can send coins to another user or to the ENCOINS Ledger script for use in the Ledger Mode."
    True ]

welcomeLedger :: [WelcomeItem]
welcomeLedger =
  [ WelcomeItem "welcome-ledger"
    "You are now in the Ledger mode! \
    \The ENCOINS Ledger is the on-chain script that acts as your stealth account. \
    \Sending coins to another user is as simple as sharing the \
    \minting keys off-chain. To receive a coin, import the minting key \
    \and re-mint the coin to gain the full control of it."
    False
  , WelcomeItem "welcome-ledger-coins"
    "Here, you can see the known coins that are stored on the ENCOINS Ledger. \
    \To select the coins to burn in the current transaction, check the boxes on the left."
    True
  , WelcomeItem "welcome-ledger-mint"
    "Here, you can add new coins to mint as in the Wallet Mode. Use the \"Add Change\" \
    \ button to auto-balance the transaction. This creates a pure transfer on \
    \ the ENCOINS Ledger without withdrawal. You can choose any wallet address as the destination \
    \ when withdrawing from the ENCOINS Ledger."
    True]

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
  when border $ performEvent_ (setElementStyle elemId "border-style" "solid" <$ eOpen)
  eClose <- dialogWindow False eOpen eClose "max-width: 700px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px; top: 0px; position: absolute;" "" $ do
    divClass "app-text-semibold" $ text message
    btn "button-switching inverted flex-center" "" $ text "Ok"
  performEvent_ (setElementStyle elemId "z-index" "" <$ eClose)
  performEvent_ (setElementStyle elemId "position" "" <$ eClose)
  performEvent_ (setElementStyle elemId "pointer-events" "" <$ eClose)
  when border $ performEvent_ (setElementStyle elemId "border-style" "none" <$ eClose)
  return eClose

welcomeWindowWalletStorageKey :: Text
welcomeWindowWalletStorageKey = "encoins-welcome-window-seen-wallet"

welcomeWindowTransferStorageKey :: Text
welcomeWindowTransferStorageKey = "encoins-welcome-window-seen-transfer"

welcomeWindowLedgerStorageKey :: Text
welcomeWindowLedgerStorageKey = "encoins-welcome-window-seen-ledger"

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
