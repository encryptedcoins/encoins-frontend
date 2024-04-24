{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Common.Widgets.Wallet where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import JS.App (walletLoad)
import Reflex.Dom hiding (Input)

import Backend.Protocol.Types (checkEmptyText, mkAddressFromPubKeys)
import Backend.Wallet
import CSL (TransactionUnspentOutputs)
import Config.Config (NetworkId (..), toNetworkId)
import ENCOINS.App.Widgets.Basic (elementResultJS)
import ENCOINS.Common.Widgets.Basic (image)

loadWallet :: (MonadWidget t m) => Event t WalletName -> m (Dynamic t Wallet)
loadWallet eWalletName = mdo
    performEvent_ (walletLoad . toJS <$> eWalletName)
    dWalletName <- elementResultJS "walletNameElement" fromJS
    eWalletNetworkId <- updated <$> elementResultJS "networkIdElement" id
    dWalletNetworkId <-
        foldDynMaybe
            (\n _ -> if T.null n then Nothing else Just $ toNetworkId n)
            Testnet
            eWalletNetworkId
    dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
    dPubKeyHash <- elementResultJS "pubKeyHashElement" id
    dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
    dUTXOs <-
        elementResultJS
            "utxosElement"
            (fromMaybe [] . decodeText :: Text -> TransactionUnspentOutputs)
    let dAddrWallet =
            zipDynWith mkAddressFromPubKeys dPubKeyHash (checkEmptyText <$> dStakeKeyHash)
    return $
        Wallet
            <$> dWalletName
            <*> dWalletNetworkId
            <*> dWalletAddressBech32
            <*> dAddrWallet
            <*> dUTXOs

walletIcon :: (MonadWidget t m) => WalletName -> m ()
walletIcon w = case w of
    None -> blank
    name -> void $ image (pure $ toJS name <> ".svg") "wallet-image" "30px"
