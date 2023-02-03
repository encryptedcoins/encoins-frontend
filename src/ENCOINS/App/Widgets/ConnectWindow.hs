module ENCOINS.App.Widgets.ConnectWindow (connectWindow) where

import           Data.Bool                     (bool)
import           Reflex.Dom

import           Backend.Wallet                (Wallet (..), toJS)
import           ENCOINS.Website.Widgets.Basic (image)
import           Widgets.Utils                 (toText)

walletEntry :: MonadWidget t m => Wallet -> m (Event t Wallet)
walletEntry w = do
    (e, _) <- elAttr' "div" ("class" =: "connect-wallet-div") $ do
        divClass "app-text-normal" $ text $ toText w
        elAttr "a" ("href" =: "#" <> "class" =: "w-inline-block") $
            image (toJS w <> ".svg") "wallet-image" "30px"
    return (w <$ domEvent Click e)

connectWindow :: MonadWidget t m => Dynamic t Bool -> m (Event t (), Dynamic t Wallet)
connectWindow isOpened =
    let mkClass b = "class" =: "container-app-fixed w-container" <> bool ("style" =: "display: none") mempty b
    in elDynAttr "div" (fmap mkClass isOpened) $
        divClass "connect-div" $ do
            eCross <- divClass "connect-title-div" $ do
                divClass "app-text-semibold" $ text "Connect Wallet"
                domEvent Click . fst <$> elAttr' "div" ("class" =: "cross-div cross-div-inverted") blank
            eEternl <- walletEntry Eternl
            dWallet <- holdDyn None $ leftmost [eEternl]
            let eConnectClose = leftmost [eCross, () <$ updated dWallet]
            return (eConnectClose, dWallet)