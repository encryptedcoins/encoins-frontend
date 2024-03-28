{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.ConnectWindow (connectWindow) where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import           Reflex.Dom

import           Backend.Utility                 (toText)
import           Backend.Wallet                  (Wallet (..), WalletName (..),
                                                  fromJS, toJS)
import           ENCOINS.App.Widgets.Basic       (loadAppDataE, saveAppData_)
import           ENCOINS.Common.Cache            (currentWallet)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Wallet   (loadWallet, walletIcon)

walletEntry :: MonadWidget t m => WalletName -> m (Event t WalletName)
walletEntry w = do
    (e, _) <- elAttr' "div" ("class" =: "connect-wallet-div") $ do
        divClass "app-text-normal" $ text $ bool "Disconnect" (toText w) $ w /= None
        elAttr "a" ("href" =: "#" <> "class" =: "w-inline-block" <>
            "style" =: "margin-left:150px;") $ bool blank (walletIcon w) $ w /= None
    return (w <$ domEvent Click e)

connectWindow :: MonadWidget t m => [WalletName] -> Event t () -> m (Dynamic t Wallet)
connectWindow supportedWallets eConnectOpen = mdo
    (eConnectClose, dWallet) <- dialogWindow True eConnectOpen eConnectClose "" "Connect Wallet" $ mdo
        eWalletName <- leftmost . ([eLastWalletName] ++) <$> mapM walletEntry supportedWallets
        eUpdate <- tag bWalletName <$> tickLossyFromPostBuildTime 10
        dW <- loadWallet (leftmost [eWalletName, eUpdate]) >>= holdUniqDyn
        let bWalletName = current $ fmap walletName dW

        -- save/load wallet
        saveAppData_ Nothing currentWallet $ toJS <$> eWalletName
        eLastWalletName <- updated <$> loadAppDataE
          Nothing currentWallet "connectWindow-key-currentWallet" fromJS None

        return (void eWalletName, dW)
    return dWallet
