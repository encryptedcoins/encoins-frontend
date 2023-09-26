module ENCOINS.App.Widgets.TabsSelection where

import           Data.Bool                              (bool)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp)
import           ENCOINS.Common.Widgets.Basic           (divClassId, btnWithBlock)

data AppTab = WalletTab | TransferTab | LedgerTab deriving (Eq, Show)

tabsSection :: MonadWidget t m
  => Dynamic t AppTab
  -> Dynamic t Bool
  -> m (Event t AppTab)
tabsSection dTab dIsDisableButtons = sectionApp "" "" $ containerApp "" $
    divClassId "app-top-menu-div" "welcome-tabs" $ do
        eWallet <- divClass "menu-item-button-right" $
            btnWithBlock (mkBtnCls WalletTab <$> dTab) "" dIsDisableButtons $ text "Wallet"
        eTransfer <- divClass "menu-item-button-right" $
            btnWithBlock (mkBtnCls TransferTab <$> dTab) "" dIsDisableButtons $ text "Transfer"
        eLedger <- divClass "menu-item-button-right" $
            btnWithBlock (mkBtnCls LedgerTab <$> dTab) "" dIsDisableButtons $ text "Ledger"
        return $ leftmost
            [ WalletTab <$ eWallet
            , TransferTab <$ eTransfer
            , LedgerTab <$ eLedger ]
    where
        mkBtnCls val cur = bool "button-not-selected" "" (val == cur)