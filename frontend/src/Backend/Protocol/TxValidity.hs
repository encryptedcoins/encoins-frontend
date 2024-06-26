{-# LANGUAGE NumericUnderscores #-}

module Backend.Protocol.TxValidity where

import Backend.Protocol.Fees (protocolFees)
import Backend.Protocol.Types
import Backend.Status
    ( AppStatus
    , isAppProcess
    )
import Backend.Utility (space, toText)
import Backend.Wallet (Wallet (..), WalletName (..), currentNetworkApp)
import CSL (TransactionUnspentOutput (..), amount, coin)
import Config.Config (NetworkConfig (..), networkConfig)
import ENCOINS.Bulletproofs (Secret (..), Secrets)
import ENCOINS.Crypto.Field (fromFieldElement)

import Data.Bool (bool)
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Reflex.Dom

data TxValidity = TxValid | TxInvalid Text
    deriving (Show, Eq)

instance Semigroup TxValidity where
    TxValid <> TxValid = TxValid
    TxValid <> TxInvalid e = TxInvalid e
    TxInvalid e <> TxValid = TxInvalid e
    TxInvalid e1 <> TxInvalid _ = TxInvalid e1

instance Monoid TxValidity where
    mempty = TxValid

txValidityWallet ::
    EncoinsMode
    -> Maybe Text
    -> Integer
    -> AppStatus
    -> Wallet
    -> Secrets
    -> Secrets
    -> TxValidity
txValidityWallet mode mUrl maxAda s Wallet{..} toBurn toMint =
    mconcat $
        zipWith
            f
            [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
            [ cond1
            , cond2
            , cond3
            , cond4
            , cond5
            , cond6
            , cond7
            , cond8
            , cond9
            , cond10
            , cond11
            , cond12
            ]
    where
        balance = getAda toMint - getAda toBurn
        fees = protocolFees mode balance
        f e = bool (TxInvalid e) TxValid
        coins = toBurn ++ toMint
        cond1 = walletName /= None
        cond2 = walletNetworkId == app networkConfig
        cond3 =
            (balance + fees + 5) * 1_000_000
                < sum (map (fromJust . decodeText . coin . amount . output) walletUTXOs)
        cond4 = not $ isAppProcess s
        cond5 = not $ null toMint
        cond6 = length coins >= 2
        cond7 = length coins <= 5
        cond8 = mode == WalletMode
        cond9 = length coins == length (nub coins)
        cond10 = maxAda + balance >= 0
        cond11 = isJust mUrl
        cond12 = mode == WalletMode
        e1 = "Connect ENCOINS to a wallet first."
        e2 =
            "Switch to the"
                <> space
                <> currentNetworkApp
                <> space
                <> "network in your wallet."
        e3 = "Not enough ADA."
        e4 = "The transaction is being processed..."
        e5 = "Minting of at least one coin in a transaction is required."
        e6 = "Minting and burning of at least two coins in a transaction is required."
        e7 = "At most five coins can be included in a Wallet Mode transaction."
        e8 =
            "At most, two coins can be minted, and two can be burned in a Ledger Mode transaction."
        e9 = "A transaction cannot include coin duplicates."
        e10 =
            "Cannot withdraw more than " <> toText maxAda <> " ADA in one transaction."
        e11 = "All available relays are down."
        e12 =
            "Transaction balance should be greater or equal to fees in the Ledger Mode."

txValidityLedger ::
    EncoinsMode
    -> Maybe Text
    -> Integer
    -> AppStatus
    -> Secrets
    -> Secrets
    -> TxValidity
txValidityLedger mode mUrl maxAda s toBurn toMint =
    mconcat $
        zipWith
            f
            [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
            [ cond1
            , cond2
            , cond3
            , cond4
            , cond5
            , cond6
            , cond7
            , cond8
            , cond9
            , cond10
            , cond11
            , cond12
            ]
    where
        balance = getAda toMint - getAda toBurn
        deposit = getDeposit toMint - getDeposit toBurn
        fees = protocolFees mode balance
        f e = bool (TxInvalid e) TxValid
        coins = toBurn ++ toMint
        cond1 = mode == LedgerMode
        cond2 = mode == LedgerMode
        cond3 = mode == LedgerMode
        cond4 = not $ isAppProcess s
        cond5 = not $ null toMint
        cond6 = length coins >= 2
        cond7 = mode == LedgerMode
        cond8 = length toMint <= 2 && length toBurn <= 2
        cond9 = length coins == length (nub coins)
        cond10 = maxAda + balance >= 0
        cond11 = isJust mUrl
        cond12 = balance + deposit + fees <= 0
        e1 = "Connect ENCOINS to a wallet first."
        e2 =
            "Switch to the"
                <> space
                <> currentNetworkApp
                <> space
                <> "network in your wallet."
        e3 = "Not enough ADA."
        e4 = "The transaction is being processed..."
        e5 = "Minting of at least one coin in a transaction is required."
        e6 = "Minting and burning of at least two coins in a transaction is required."
        e7 = "At most five coins can be included in a Wallet Mode transaction."
        e8 =
            "At most, two coins can be minted, and two can be burned in a Ledger Mode transaction."
        e9 = "A transaction cannot include coin duplicates."
        e10 =
            "Cannot withdraw more than " <> toText maxAda <> " ADA in one transaction."
        e11 = "All available relays are down."
        e12 =
            "Transaction balance should be greater or equal to fees in the Ledger Mode."

getAda :: Secrets -> Integer
getAda = sum . map (fromFieldElement . secretV)

getDeposit :: Secrets -> Integer
getDeposit = (* 4) . getCoinNumber

getCoinNumber :: Secrets -> Integer
getCoinNumber = fromIntegral . length
