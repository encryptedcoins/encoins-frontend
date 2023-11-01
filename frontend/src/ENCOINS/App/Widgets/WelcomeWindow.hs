{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.WelcomeWindow where

import           Control.Monad                   (when)
import           Data.Text                       (Text)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic       (loadJsonFromStorage,
                                                  saveJsonToStorage)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn, lnkInlineInverted)
import           JS.Website                      (setElementStyle)

data WelcomeItem m = WelcomeItem
  { elemId  :: Text
  , border  :: Bool
  , message :: m ()
  }

welcomeWallet :: MonadWidget t m => [WelcomeItem m]
welcomeWallet =
  [ WelcomeItem "welcome-tabs"
    False
    $ text "Welcome to ENCOINS! You can select the mode of operation from this menu."
  , WelcomeItem "welcome-wallet-coins"
    True
    $ text "In the left column, you see the coins from your wallet. \
     \Clicking on a particular coin reveals its full name and asset fingerprint. \
     \To select coins to burn in the current transaction, check the boxes on the left. \
     \To copy the minting key, click on the key icon."
  , WelcomeItem "welcome-coins-mint"
    True
    $ text "Here, you can add new coins to mint by entering their ADA value and clicking \
    \the \"+\" button (or by pressing \"Enter\" on the keyboard). You can remove a coin from the current transaction by \
    \clicking the \"x\" button."
  , WelcomeItem "welcome-tx-balance"
    False
    $ text "Here, you can see the net ADA balance of the current transaction and protocol fees. A positive balance means you are withdrawing ADA from the protocol."
  , WelcomeItem "welcome-send-req"
    True
    $ text "Once you finished building the transaction, you can press \"SEND REQUEST\" button to execute it."
  , WelcomeItem "welcome-import-export"
    False
    $ text "All known coins are stored locally on your device. Here, you can import \
    \ the coins that are new to this device. You can also export your \
    \ coins for backup or use on another device."
  , WelcomeItem "welcome-read-docs"
    False
    $ do
      text "Full user documentation is always available at "
      lnkInlineInverted "https://docs.encoins.io" "docs.encoins.io"
      text ". Visit our Discord community at "
      lnkInlineInverted "https://discord.gg/Q3gPP87Tcw" "discord.gg/Q3gPP87Tcw"
      text ". Stay up to date with the project's news on Twitter at "
      lnkInlineInverted "https://twitter.com/ENCOINS1" "twitter.com/ENCOINS1"
      text "."
    ]

welcomeTransfer :: MonadWidget t m => [WelcomeItem m]
welcomeTransfer =
  [ WelcomeItem "welcome-coins-transfer"
    True
    $ text "You are now in the transfer mode! To select the coins to transfer, \
    \check the boxes on the left."
  , WelcomeItem "welcome-transfer-btns"
    True
    $ text "You can send coins to another user or to the ENCOINS Ledger script for use in the Ledger Mode."
    ]

welcomeLedger :: MonadWidget t m => [WelcomeItem m]
welcomeLedger =
  [ WelcomeItem "welcome-ledger"
    False
    $ text "You are now in the Ledger mode! \
    \The ENCOINS Ledger is the on-chain script that acts as your stealth account. \
    \Sending coins to another user is as simple as sharing the \
    \minting keys off-chain. To receive a coin, import the minting key \
    \and re-mint the coin to gain the full control of it."
  , WelcomeItem "welcome-ledger-coins"
    True
    $ text "Here, you can see the known coins that are stored on the ENCOINS Ledger. \
    \To select the coins to burn in the current transaction, check the boxes on the left."
  , WelcomeItem "welcome-ledger-mint"
    True
    $ text "Here, you can add new coins to mint as in the Wallet Mode. Use the \"Add Change\" \
    \ button to auto-balance the transaction. This creates a pure transfer on \
    \ the ENCOINS Ledger without withdrawal. You can choose any wallet address as the destination \
    \ when withdrawing from the ENCOINS Ledger."
    ]

welcomeTutorial :: MonadWidget t m => [WelcomeItem m] -> Event t () -> m (Event t ())
welcomeTutorial [] eOpen = pure eOpen
welcomeTutorial (wi:wis) eOpen = do
  eWiNext <- welcomeItemWidget wi eOpen
  eOpenNext <- delay 0.3 eWiNext
  welcomeTutorial wis eOpenNext

welcomeItemWidget :: MonadWidget t m => WelcomeItem m -> Event t () -> m (Event t ())
welcomeItemWidget WelcomeItem{..} eOpen = mdo
  performEvent_ (setElementStyle elemId "z-index" "2000" <$ eOpen)
  performEvent_ (setElementStyle elemId "position" "relative" <$ eOpen)
  performEvent_ (setElementStyle elemId "pointer-events" "none" <$ eOpen)
  when border $ performEvent_ (setElementStyle elemId "border-style" "solid" <$ eOpen)
  eClose <- dialogWindow False eOpen eClose "max-width: 700px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px; top: 0px; position: absolute;" "" $ do
    divClass "app-text-semibold" message
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

welcomeWindow :: MonadWidget t m => Text -> [WelcomeItem m] -> m ()
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
