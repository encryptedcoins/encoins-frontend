module Backend.Status where

import           Data.Text                    (Text, unpack)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic    (elementResultJS)

data Status =
      Ready             -- ^ The default status
    | Constructing      -- ^ Transaction is sent to the backend for constructing and balancing
    | Signing           -- ^ Transaction is sent to the wallet for signing
    | Submitting        -- ^ Transaction is sent to the backend for submission
    | Submitted         -- ^ Transaction is submitted to the blockchain
    | BackendError Text
    | WalletError Text
    | CustomStatus Text
    deriving Eq

instance Show Status where
    show Ready            = ""
    show Constructing     = "Constructing the transaction..."
    show Signing          = "Please sign the transaction."
    show Submitting       = "Submitting..."
    show Submitted        = "The transaction is now pending..."
    show (BackendError e) = "Error: " <> unpack e
    show (WalletError e)  = "Error: " <> unpack e
    show (CustomStatus t) = unpack t

isStatusBusy :: Status -> Bool
isStatusBusy Constructing = True
isStatusBusy Signing      = True
isStatusBusy Submitting   = True
isStatusBusy Submitted    = True
isStatusBusy _            = False

-- Wallet error element
walletError :: MonadWidget t m => m (Event t Status)
walletError = do
    dWalletError <- elementResultJS "walletErrorElement" id
    let eWalletError = ffilter ("" /=) $ updated dWalletError
    return $ WalletError <$> eWalletError

-- Other error element
otherError :: MonadWidget t m => Event t Text -> m (Event t Status)
otherError eOtherError = do
    let eOtherErrorNonEmpty = ffilter ("" /=) eOtherError
    return $ WalletError <$> eOtherErrorNonEmpty