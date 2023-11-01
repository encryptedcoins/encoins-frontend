{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.SendToWalletWindow where

import           Reflex.Dom

import           Backend.Protocol.Utility        (secretToHex)
import           ENCOINS.Bulletproofs            (Secrets)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (br, btn)

sendToWalletWindow :: MonadWidget t m => Event t () -> Dynamic t Secrets -> m (Event t ())
sendToWalletWindow eOpen dSecrets = mdo
  (eOk, eCancel) <- dialogWindow True eOpen (leftmost [eOk,eCancel]) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" "" $ do
      divClass "connect-title-div" $ divClass "app-text-semibold" $
          text "Copy and send these keys to your recepient off-chain:"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between;text-align:left;") $
          dyn_ $ mapM ((>> br) . text . secretToHex) <$> dSecrets
      br
      btnOk <- btn "button-switching inverted flex-center" "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      btnCancel <- btn "button-switching flex-center" "width:30%;display:inline-block;margin-left:5px;" $ text "Cancel"
      return (btnOk, btnCancel)
  return eOk
