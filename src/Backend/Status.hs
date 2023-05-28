module Backend.Status where

import           Data.Text                    (Text, unpack)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic    (elementResultJS)

data Status =
      Ready             -- ^ The default status
    | Balancing         -- ^ Transaction is sent to the backend for balancing
    | Signing           -- ^ Transaction is sent to the wallet for signing
    | Submitting        -- ^ Transaction is sent to the backend for submission
    | Submitted         -- ^ Transaction is submitted to the blockchain
    | BackendError Text
    | WalletError Text
    deriving Eq

instance Show Status where
    show Ready            = ""
    show Balancing        = "Balancing..."
    show Signing          = "Please sign the transaction."
    show Submitting       = "Submitting..."
    show Submitted        = "The transaction is now pending..."
    show (BackendError e) = "Error: " <> unpack e
    show (WalletError e)  = "Error: " <> unpack e

isStatusBusy :: Status -> Bool
isStatusBusy Balancing  = True
isStatusBusy Signing    = True
isStatusBusy Submitting = True
isStatusBusy Submitted  = True
isStatusBusy _          = False

-- Wallet error element
walletError :: MonadWidget t m => m (Event t Status)
walletError = do
    dWalletError <- elementResultJS "walletErrorElement" id
    let eWalletError = ffilter ("" /=) $ updated dWalletError
    return $ WalletError <$> eWalletError