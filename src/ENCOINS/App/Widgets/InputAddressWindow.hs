module ENCOINS.App.Widgets.InputAddressWindow where

import           Control.Monad                          (void)
import           Reflex.Dom
import           Witherable                             (catMaybes)

import           Backend.Types
import           ENCOINS.App.Widgets.Basic              (elementResultJS)
import           ENCOINS.Common.Widgets.Basic           (btn, errDiv)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           JS.App                                 (addrLoad)

inputAddressWindow :: MonadWidget t m => Event t () -> m (Event t Address)
inputAddressWindow eOpen = mdo
  eOk <- dialogWindow True eOpen (void eOk) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ do
      divClass "connect-title-div" $ divClass "app-text-semibold" $
          text "Enter wallet address in bech32:"
      dAddrInp <- divClass "app-columns w-row" $ do
        inp <- inputElement $ def
          & initialAttributes .~ ("class" =: "w-input" <> "style" =: "display: inline-block;")
          & inputElementConfig_initialValue .~ ""
          & inputElementConfig_setValue .~ ("" <$ eOpen)
        return (value inp)
      btnOk <- btn "button-switching inverted flex-center" "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      performEvent_ (addrLoad <$> tagPromptlyDyn dAddrInp btnOk)
      dPubKeyHash <- elementResultJS "addrPubKeyHashElement" id
      dStakeKeyHash <- elementResultJS "addrStakeKeyHashElement" id
      let
        dmAddr = zipDynWith mkAddr (checkEmptyText <$> dPubKeyHash) (checkEmptyText <$> dStakeKeyHash)
        emRes = tagPromptlyDyn dmAddr btnOk
      widgetHold_ blank $ leftmost [maybe err (const blank) <$> emRes, blank <$ eOpen]
      return (catMaybes emRes)
  return eOk
  where
    mkAddr Nothing _ = Nothing
    mkAddr (Just pkh) mskh = Just $ mkAddressFromPubKeys pkh mskh
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect address"