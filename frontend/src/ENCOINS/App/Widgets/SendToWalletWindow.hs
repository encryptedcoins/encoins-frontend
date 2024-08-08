{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.SendToWalletWindow where

import Reflex.Dom

import Common.Protocol (secretToHex)
import ENCOINS.Bulletproofs (Secrets)
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Basic (br, btn)
import I18n.I18n (App)
import qualified I18n.I18n as I18n

sendToWalletWindow ::
    (App t m) => Event t () -> Dynamic t Secrets -> m (Event t ())
sendToWalletWindow eOpen dSecrets = mdo
    (eOk, eCancel) <- dialogWindow
        True
        eOpen
        (leftmost [eOk, eCancel])
        "app-SendToWalletWindow"
        I18n.EmptyTerm $ do
        divClass "connect-title-div" $
            divClass "app-text-semibold" $
                text "Copy and send these keys to your recepient off-chain:"
        divClass "app-Transfer_SendToWalletWindow_Secret" $
            dyn_ $
                mapM ((>> br) . text . secretToHex) <$> dSecrets
        br
        btnOk <-
            btn
                "button-switching inverted flex-center"
                "width:30%;display:inline-block;margin-right:5px;"
                $ text "Ok"
        btnCancel <-
            btn
                "button-switching flex-center"
                "width:30%;display:inline-block;margin-left:5px;"
                $ text "Cancel"
        return (btnOk, btnCancel)
    return eOk
