module ENCOINS.App.Widgets.TabsSelection where

import           Data.Bool                    (bool)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic    (containerApp, sectionApp)
import           ENCOINS.Common.Widgets.Basic (btnWithBlock, divClassId)

data AppTab = WalletTab | TransferTab | LedgerTab deriving (Eq, Show)

tabsSection :: MonadWidget t m
  => Dynamic t AppTab
  -> Dynamic t Bool
  -> m (Event t AppTab)
tabsSection dTab dIsDisableButtons = sectionApp "" "" $ containerApp "" $
    divClassId "app-tab-menu" "welcome-tabs" $ do
        eWallet <- divClass "menu-tab-item-button" $
            btnWithBlock (mkBtnCls WalletTab <$> dTab) "width:100%" dIsDisableButtons $ text "Wallet"
        eTransfer <- divClass "menu-tab-item-button" $
            btnWithBlock (mkBtnCls TransferTab <$> dTab) "width:100%" dIsDisableButtons $ text "Transfer"
        eLedger <- divClass "menu-tab-item-button" $
            btnWithBlock (mkBtnCls LedgerTab <$> dTab) "width:100%" dIsDisableButtons $ text "Ledger"
        return $ leftmost
            [ WalletTab <$ eWallet
            , TransferTab <$ eTransfer
            , LedgerTab <$ eLedger ]
    where
        mkBtnCls val cur = bool "button-not-selected" "" (val == cur)
