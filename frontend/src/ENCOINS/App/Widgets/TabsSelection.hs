module ENCOINS.App.Widgets.TabsSelection where

import Data.Bool (bool)
import Reflex.Dom

import ENCOINS.Common.Widgets.Basic (btnWithBlock, containerApp, divClassId, sectionApp)
import I18n.Reflex.I18n (App)
import Common.Reflex.Dom.Extra (textLocale)
import I18n.App (AppMessage(..))

data AppTab
    = WalletTab
    | TransferTab
    | LedgerTab
    deriving (Eq, Show)

tabsSection ::
    (App t m) =>
    Dynamic t AppTab
    -> Dynamic t Bool
    -> m (Event t AppTab)
tabsSection dTab dIsDisableButtons = sectionApp "" "" $
    containerApp "" $
        divClassId "app-tab-menu" "welcome-tabs" $ do
            eWallet <-
                divClass "menu-tab-item-button" $
                    btnWithBlock (mkBtnCls WalletTab <$> dTab) "width:100%" dIsDisableButtons $
                        textLocale TabWallet
            eTransfer <-
                divClass "menu-tab-item-button" $
                    btnWithBlock (mkBtnCls TransferTab <$> dTab) "width:100%" dIsDisableButtons $
                        textLocale TabTransfer
            eLedger <-
                divClass "menu-tab-item-button" $
                    btnWithBlock (mkBtnCls LedgerTab <$> dTab) "width:100%" dIsDisableButtons $
                        textLocale TabLedger
            return $
                leftmost
                    [ WalletTab <$ eWallet
                    , TransferTab <$ eTransfer
                    , LedgerTab <$ eLedger
                    ]
    where
        mkBtnCls val cur = bool "button-not-selected" "" (val == cur)
