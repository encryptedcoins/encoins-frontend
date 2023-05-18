{-# LANGUAGE NumericUnderscores #-}

module ENCOINS.App.Protocol.TxValidity where

import           Data.Bool                        (bool)
import           Data.List                        (nub)
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Reflex.Dom

import           Backend.Status                   (Status(..))
import           Backend.Types
import           Backend.Wallet                   (Wallet (..), WalletName (..))
import           CSL                              (TransactionUnspentOutput(..), amount, coin)
import           ENCOINS.Bulletproofs             (Secrets, Secret (..))
import           ENCOINS.Crypto.Field             (fromFieldElement)
import           Widgets.Utils                    (toText)

data TxValidity = TxValid | TxInvalid Text
    deriving (Show, Eq)

instance Semigroup TxValidity where
    TxValid <> TxValid = TxValid
    TxValid <> TxInvalid e = TxInvalid e
    TxInvalid e <> TxValid = TxInvalid e
    TxInvalid e1 <> TxInvalid _ = TxInvalid e1

instance Monoid TxValidity where
    mempty = TxValid

txValidity :: EncoinsMode -> Integer -> Status -> Wallet -> Secrets -> Secrets -> TxValidity
txValidity mode maxAda s Wallet{..} toBurn toMint = mconcat $ zipWith f
        [e7, e8, e6, e0, e1, e2, e3, e4, e5, e9]
        [cond7, cond8, cond6, cond0, cond1, cond2, cond3, cond4, cond5, cond9]
    where
        getBalance = sum . map (fromFieldElement . secretV)
        balance = getBalance toMint - getBalance toBurn
        f e = bool (TxInvalid e) TxValid
        coins = toBurn ++ toMint
        cond0 = s `notElem` [Balancing, Signing, Submitting, Submitted]
        cond1 = walletName /= None
        cond2 = not $ null toMint
        cond3 = length coins >= 2
        cond4 = length coins <= 5
        cond5 = length coins == length (nub coins)
        cond6 = (balance + 5) * 1_000_000 < sum (map (fromJust . decodeText . coin . amount . output) walletUTXOs)
        cond7 = walletName /= None
        cond8 = walletNetworkId == "0"
        cond9 = maxAda + balance >= 0
        e0    = "The transaction is being processed."
        e1    = "Connect ENCOINS DApp to a wallet first."
        e2    = "Minting at least one coin is required to preserve privacy."
        e3    = "At least two coins must be included in a transaction to preserve privacy."
        e4    = "At most five coins can be included in a transaction."
        e5    = "A transaction cannot include coin duplicates."
        e6    = "Not enough ADA."
        e7    = "Connect ENCOINS to a wallet first."
        e8    = "Switch to the Testnet Preprod network in your wallet."
        e9    = "Cannot withdraw more than " <> toText maxAda <> " ADA in one transaction."