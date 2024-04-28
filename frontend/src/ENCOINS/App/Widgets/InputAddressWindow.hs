{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.InputAddressWindow where

import Control.Monad (void)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Reflex.Dom
import Witherable (catMaybes)

import Backend.Protocol.Types
import Config.Config (NetworkConfig (..), NetworkId (..), networkConfig)
import ENCOINS.App.Widgets.Basic (elementResultJS)
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Basic (btnWithBlock, errDiv)
import JS.App (addrLoad)

inputAddressWindow ::
    (MonadWidget t m) => Event t () -> m (Event t Address, Dynamic t (Maybe Address))
inputAddressWindow eOpen = mdo
    (eOk, dmAddress) <- dialogWindow True eOpen (void eOk) "app-InputAddressWindow" "" $ mdo
        divClass "connect-title-div" $
            divClass "app-text-semibold" $
                text "Enter wallet address in bech32:"
        dAddrInp <- divClass "app-columns w-row" $ do
            inp <-
                inputElement $
                    def
                        & initialAttributes
                        .~ ( "class" =: "w-input"
                                <> "style" =: "display: inline-block;"
                                <> "placeholder" =: addressBech32
                           )
                        & inputElementConfig_initialValue
                        .~ ""
                        & inputElementConfig_setValue
                        .~ ("" <$ eOpen)
            setFocusDelayOnEvent inp eOpen
            return (value inp)
        performEvent_ (addrLoad <$> updated dAddrInp)
        dPubKeyHash <- elementResultJS "addrPubKeyHashElement" id
        dStakeKeyHash <- elementResultJS "addrStakeKeyHashElement" id
        let dmAddr =
                zipDynWith
                    mkAddr
                    (traceDyn "dPubKeyHash" $ checkEmptyText <$> dPubKeyHash)
                    (traceDyn "dStakeKeyHash" $ checkEmptyText <$> dStakeKeyHash)
            emRes = traceEvent "emRes" $ tagPromptlyDyn dmAddr btnOk
        btnOk <-
            btnWithBlock
                btnAttrs
                "width:30%;display:inline-block;margin-right:5px;"
                (isNothing <$> dmAddr)
                (text "Ok")
        widgetHold_ blank $ leftmost [maybe err (const blank) <$> emRes, blank <$ eOpen]
        return (catMaybes emRes, dmAddr)
    return (eOk, dmAddress)
    where
        btnAttrs = "button-switching inverted flex-center"
        mkAddr Nothing _ = Nothing
        mkAddr (Just pkh) mskh = Just $ mkAddressFromPubKeys pkh mskh
        err =
            elAttr
                "div"
                ( "class" =: "app-columns w-row"
                    <> "style" =: "display:flex;justify-content:center;"
                )
                $ errDiv "Incorrect address"

addressBech32 :: Text
addressBech32 = case app networkConfig of
    Mainnet -> "addr1q88cdsle3chjufssrg9wujvseypyj8fgx..."
    Testnet -> "addr_test1qr8cdsle3chjufssrg9wujvseypy..."
