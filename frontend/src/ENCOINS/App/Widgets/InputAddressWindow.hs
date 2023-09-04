module ENCOINS.App.Widgets.InputAddressWindow where

import           Control.Monad                          (void)
import           Reflex.Dom
import           Witherable                             (catMaybes)

import           Backend.Protocol.Types
import           ENCOINS.App.Widgets.Basic              (elementResultJS)
import           ENCOINS.Common.Widgets.Basic           (btn, errDiv)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           JS.App                                 (addrLoad)

inputAddressWindow :: MonadWidget t m => Event t () -> m (Event t Address, Dynamic t (Maybe Address))
inputAddressWindow eOpen = mdo
  (eOk, dmAddress) <- dialogWindow True eOpen (void eOk) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" "" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $
          text "Enter wallet address in bech32:"
      dAddrInp <- divClass "app-columns w-row" $ do
        inp <- inputElement $ def
          & initialAttributes .~ ("class" =: "w-input" <> "style" =: "display: inline-block;")
          & inputElementConfig_initialValue .~ ""
          & inputElementConfig_setValue .~ ("" <$ eOpen)
        return (value inp)
      btnOk <- btn (mkBtnAttrs <$> dmAddr)
        "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      performEvent_ (addrLoad <$> updated dAddrInp)
      dPubKeyHash <- elementResultJS "addrPubKeyHashElement" id
      dStakeKeyHash <- elementResultJS "addrStakeKeyHashElement" id
      let
        dmAddr = zipDynWith mkAddr
          (traceDyn "dPubKeyHash" $ checkEmptyText <$> dPubKeyHash)
          (traceDyn "dStakeKeyHash" $ checkEmptyText <$> dStakeKeyHash)
        emRes = traceEvent "emRes" $ tagPromptlyDyn dmAddr btnOk
      widgetHold_ blank $ leftmost [maybe err (const blank) <$> emRes, blank <$ eOpen]
      return (catMaybes emRes, dmAddr)
  return (eOk, dmAddress)
  where
    btnAttrs = "button-switching inverted flex-center"
    mkBtnAttrs maddr = btnAttrs <> maybe " button-disabled" (const "") maddr
    mkAddr Nothing _ = Nothing
    mkAddr (Just pkh) mskh = Just $ mkAddressFromPubKeys pkh mskh
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect address"
