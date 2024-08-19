{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Common.ConnectWindow
    ( connectWindow
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Reflex.Dom

import Backend.Wallet (Wallet (..), WalletName (..), fromJS, toJS)
import Common.Utility (toText)
import ENCOINS.Common.Cache (currentWallet, loadAppDataE, saveAppData_)
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Wallet (loadWallet, walletIcon)
import qualified I18n.Common as I18n
import I18n.I18n (App)
import qualified I18n.I18n as I18n

viewWalletEntry :: (MonadWidget t m) => WalletName -> m (Event t WalletName)
viewWalletEntry w = do
    (e, _) <- elAttr' "div" ("class" =: "connect-wallet-div") $ do
        divClass "app-text-normal" $ text $ bool "Disconnect" (toText w) $ w /= None
        elAttr
            "a"
            ( "href" =: "#"
                <> "class" =: "w-inline-block"
                <> "style" =: "margin-left:150px;"
            )
            $ bool blank (walletIcon w)
            $ w /= None
    return (w <$ domEvent Click e)

connectWindow ::
    (App t m) => [WalletName] -> Event t () -> m (Dynamic t Wallet)
connectWindow supportedWallets eConnectOpen = mdo
    (eConnectClose, dWallet) <- dialogWindow
        True
        eConnectOpen
        eConnectClose
        "common-ConnectWindow"
        (I18n.CommonTerm I18n.ConnectWalletWindowTitle)
        $ mdo
            eWalletName <-
                divClass "common-Connect_WalletContainer" $
                    leftmost . ([eLastWalletName] ++) <$> mapM viewWalletEntry supportedWallets
            eUpdate <- tag bWalletName <$> tickLossyFromPostBuildTime 10
            dW <- loadWallet (leftmost [eWalletName, eUpdate]) >>= holdUniqDyn
            let bWalletName = current $ fmap walletName dW

            -- save/load wallet
            saveAppData_ Nothing currentWallet $ toJS <$> eWalletName
            eLastWalletName <-
                updated
                    <$> loadAppDataE
                        Nothing
                        currentWallet
                        "connectWindow-key-currentWallet"
                        fromJS
                        None

            return (void eWalletName, dW)
    return dWallet
