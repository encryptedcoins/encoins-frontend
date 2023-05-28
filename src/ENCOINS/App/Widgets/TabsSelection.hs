module ENCOINS.App.Widgets.TabsSelection where

import           Data.Bool                              (bool)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)

data AppTab = WalletTab | TransferTab | LedgerTab deriving (Eq, Show)

tabsSection :: MonadWidget t m => Dynamic t AppTab -> m (Event t AppTab)
tabsSection dTab = sectionApp "" "" $ containerApp "" $
    divClassId "app-top-menu-div" "welcome-tabs" $ do
        eWallet <- divClass "menu-item-button-right" $
            btn (mkBtnCls WalletTab <$> dTab) "" $ text "Wallet"
        eTransfer <- divClass "menu-item-button-right" $
            btn (mkBtnCls TransferTab <$> dTab) "" $ text "Transfer"
        eLedger <- divClass "menu-item-button-right" $
            btn (mkBtnCls LedgerTab <$> dTab) "" $ text "Ledger"
        return $ leftmost
            [ WalletTab <$ eWallet
            , TransferTab <$ eTransfer
            , LedgerTab <$ eLedger ]
    where
        mkBtnCls val cur = bool "button-not-selected" "" (val == cur)