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
        divClass "connect-div" $ do
            eCross <- divClass "connect-title-div" $ do
                divClass "app-text-semibold" $ text "Connect Wallet"
                domEvent Click . fst <$> elAttr' "div" ("class" =: "cross-div cross-div-inverted") blank
            eEternl <- walletEntry Eternl
            eNami   <- walletEntry Nami
            eFlint  <- walletEntry Flint
            eNone   <- walletEntry None
            let eW  = leftmost [eNone, eEternl, eNami, eFlint]
                eCC = leftmost [eCross, () <$ eW]
            dW <- loadWallet eW
            return (eCC, dW)
    return dWallet