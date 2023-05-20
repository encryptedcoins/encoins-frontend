module ENCOINS.App.Widgets.ConnectWindow (connectWindow) where

import           Control.Monad                   (void)
import           Data.Aeson                      (encode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (toStrict)
import           Data.Text.Encoding              (decodeUtf8)
import           Reflex.Dom

import           Backend.Wallet                  (WalletName (..), Wallet (..), loadWallet, walletIcon, fromJS, toJS)
import           ENCOINS.App.Widgets.Basic       (loadAppData)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           JS.Website                      (saveJSON)
import           Widgets.Utils                   (toText)

walletEntry :: MonadWidget t m => WalletName -> m (Event t WalletName)
walletEntry w = do
    (e, _) <- elAttr' "div" ("class" =: "connect-wallet-div") $ do
        divClass "app-text-normal" $ text $ bool "Disconnect" (toText w) $ w /= None
        elAttr "a" ("href" =: "#" <> "class" =: "w-inline-block" <>
            "style" =: "margin-left:30px;") $ bool blank (walletIcon w) $ w /= None
    return (w <$ domEvent Click e)

connectWindow :: MonadWidget t m => Event t () -> m (Dynamic t Wallet)
connectWindow eConnectOpen = mdo
    (eConnectClose, dWallet) <- dialogWindow True eConnectOpen eConnectClose "" $ mdo
        eWalletName <- leftmost . ([eLastWalletName] ++) <$> mapM walletEntry [minBound..maxBound]
        eUpdate <- tag bWalletName <$> tickLossyFromPostBuildTime 10
        dW <- loadWallet (leftmost [eWalletName, eUpdate]) >>= holdUniqDyn
        let bWalletName = current $ fmap walletName dW

        -- save/load wallet
        performEvent_ (saveJSON Nothing "current-wallet" . decodeUtf8 . toStrict . encode . toJS <$> eWalletName)
        eLastWalletName <- updated <$> loadAppData Nothing "current-wallet" fromJS None

        return (void eWalletName, dW)
    return dWallet
