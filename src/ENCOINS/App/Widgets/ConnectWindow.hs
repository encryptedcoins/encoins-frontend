module ENCOINS.App.Widgets.ConnectWindow (connectWindow) where

import           Data.Bool                     (bool)
import           Reflex.Dom

import           Backend.Wallet                (WalletName (..), Wallet (..), loadWallet, walletIcon)
import           Widgets.Utils                 (toText)

walletEntry :: MonadWidget t m => WalletName -> m (Event t WalletName)
walletEntry w = do
    (e, _) <- elAttr' "div" ("class" =: "connect-wallet-div") $ do
        divClass "app-text-normal" $ text $ toText w
        elAttr "a" ("href" =: "#" <> "class" =: "w-inline-block") $
            bool blank (walletIcon w) $ w /= None
    return (w <$ domEvent Click e)

connectWindow :: MonadWidget t m => Event t () -> m (Dynamic t Wallet)
connectWindow eConnectOpen = mdo
    dConnectIsOpen <- holdDyn False $ leftmost [True <$ eConnectOpen, False <$ eConnectClose]
    let mkClass b = "class" =: "div-app-fixed" <> bool ("style" =: "display: none") mempty b
    (eConnectClose, dWallet) <- elDynAttr "div" (fmap mkClass dConnectIsOpen) $
        divClass "connect-div" $ mdo
            eCross <- divClass "connect-title-div" $ do
                divClass "app-text-semibold" $ text "Connect Wallet"
                domEvent Click . fst <$> elAttr' "div" ("class" =: "cross-div cross-div-inverted") blank
            eEternl <- walletEntry Eternl
            eNami   <- walletEntry Nami
            eFlint  <- walletEntry Flint
            eNuFi   <- walletEntry NuFi
            eGero   <- walletEntry Gero
            eBegin  <- walletEntry Begin
            eTyphon <- walletEntry Typhon
            eLace   <- walletEntry Lace
            eNone   <- walletEntry None
            let eW  = leftmost [eNone, eEternl, eNami, eFlint, eNuFi, eGero, eBegin, eTyphon, eLace]
                eCC = leftmost [eCross, () <$ eW]
            eUpdate <- tag bWalletName <$> tickLossyFromPostBuildTime 10
            dW <- loadWallet (leftmost [eW, eUpdate]) >>= holdUniqDyn
            let bWalletName = current $ fmap walletName dW
            return (eCC, dW)
    return dWallet