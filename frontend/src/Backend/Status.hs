{-# LANGUAGE InstanceSigs #-}

module Backend.Status where

import Backend.Utility (column, space, toText)
import Data.Text (Text)
import qualified Data.Text as T

data AppStatus
    = AppReady
    | CustomStatus Text
    | WalletTx WalletTxStatus
    | TransferTx TransferTxStatus
    | LedgerTx LedgerTxStatus
    | CloudIcon CloudIconStatus
    | CloudRestore CloudRestoreStatus
    | Migrate MigrateStatus
    | WalletInApp WalletStatus
    deriving stock (Eq)

textAppStatus :: AppStatus -> Text
textAppStatus appStatus =
    if isAppReady appStatus
        then T.empty
        else case appStatus of
            AppReady -> T.empty
            CustomStatus t -> t
            WalletTx s -> "Wallet mode" <> column <> space <> textWalletTxStatus s
            TransferTx s -> "Transfer mode" <> column <> space <> textTransferTxStatus s
            LedgerTx s -> "Ledger mode" <> column <> space <> textLedgerTxStatus s
            CloudIcon s -> toText s
            CloudRestore s -> "Cloud" <> column <> space <> textCloudRestoreStatus s
            Migrate s -> "Migration" <> column <> space <> textMigrateStatus s
            WalletInApp s -> "Wallet" <> column <> space <> textWalletStatus s

data WalletTxStatus
    = -- Default, initial status
      WalTxReady
    | -- | Transaction is sent to the backend for constructing and balancing
      WalTxConstructing
    | -- | Transaction is sent to the wallet for signing
      WalTxSigning
    | -- | Transaction is sent to the backend for submission
      WalTxSubmitting
    | -- | Transaction is submitted to the blockchain
      WalTxSubmitted
    | -- | All relay are down. Need reload
      WalTxNoRelay
    | WalTxBackendError Text
    deriving (Eq)

textWalletTxStatus :: WalletTxStatus -> Text
textWalletTxStatus = \case
    WalTxReady -> T.empty
    WalTxConstructing -> "Constructing the transaction..."
    WalTxSigning -> "Please sign the transaction."
    WalTxSubmitting -> "Submitting..."
    WalTxSubmitted -> "Submitted. Pending the confirmation..."
    WalTxNoRelay -> "All available relays are down!"
    WalTxBackendError e -> e

data TransferTxStatus
    = -- Default, initial status
      TransTxReady
    | -- | Transaction is sent to the backend for constructing and balancing
      TransTxConstructing
    | -- | Transaction is sent to the wallet for signing
      TransTxSigning
    | -- | Transaction is sent to the backend for submission
      TransTxSubmitting
    | -- | Transaction is submitted to the blockchain
      TransTxSubmitted
    | -- | All relay are down. Need reload
      TransTxNoRelay
    | TransTxBackendError Text
    deriving (Eq)

textTransferTxStatus :: TransferTxStatus -> Text
textTransferTxStatus = \case
    TransTxReady -> T.empty
    TransTxConstructing -> "Constructing the transaction..."
    TransTxSigning -> "Please sign the transaction."
    TransTxSubmitting -> "Submitting..."
    TransTxSubmitted -> "Submitted. Pending the confirmation..."
    TransTxNoRelay -> "All available relays are down!"
    TransTxBackendError e -> "Error" <> column <> space <> e

data LedgerTxStatus
    = -- Default, initial status
      LedTxReady
    | -- | Transaction is sent to the backend for constructing and balancing
      LedTxConstructing
    | -- | Transaction is submitted to the blockchain
      LedTxSubmitted
    | -- | All relay are down. Need reload
      LedTxNoRelay
    | -- | Change address is invalid in Ledger mode
      LedTxInvalidChangeAddress
    | LedTxBackendError Text
    deriving (Eq)

textLedgerTxStatus :: LedgerTxStatus -> Text
textLedgerTxStatus = \case
    LedTxReady -> T.empty
    LedTxConstructing -> "Constructing the transaction..."
    LedTxSubmitted -> "Submitted. Pending the confirmation..."
    LedTxNoRelay -> "All available relays are down!"
    LedTxInvalidChangeAddress -> "ChangeAddress is invalid"
    LedTxBackendError e -> "Error" <> column <> space <> e

data WalletStatus
    = WalletReady
    | WalletNetworkError Text
    | WalletFail Text
    deriving (Eq)

textWalletStatus :: WalletStatus -> Text
textWalletStatus = \case
    WalletReady -> T.empty
    WalletNetworkError t -> t
    WalletFail t -> t

data MigrateStatus
    = MigReady
    | MigSuccess
    | MigUpdating
    deriving (Eq)

textMigrateStatus :: MigrateStatus -> Text
textMigrateStatus = \case
    MigReady -> T.empty
    MigSuccess -> "Local cache was updated to the last version"
    MigUpdating -> "Cache structure is updating. Please wait."

data CloudRestoreStatus = RestoreFail | RestoreSuccess Int
    deriving stock (Eq)

textCloudRestoreStatus :: CloudRestoreStatus -> Text
textCloudRestoreStatus = \case
    RestoreFail -> "Restoring tokens failed"
    RestoreSuccess n -> "Restored " <> toText n <> " tokens (with duplicates)"

data DaoStatus
    = DaoReady
    | DelegateTx DelegateTxStatus
    | VoteTx VoteTxStatus
    | WalletInDao WalletStatus
    deriving stock (Eq)

textDaoStatus :: DaoStatus -> Text
textDaoStatus daoStatus =
    if isDaoReady daoStatus
        then T.empty
        else case daoStatus of
            DaoReady -> T.empty
            DelegateTx s -> "Delegate" <> column <> space <> textDelegateTxStatus s
            VoteTx s -> "Vote" <> column <> space <> textVoteTxStatus s
            WalletInDao s -> "Wallet" <> column <> space <> textWalletStatus s

data DelegateTxStatus
    = DelTxReady
    | -- | Transaction is passed successfully
      DelTxSuccess
    | -- | Transaction is sent to the backend for constructing and balancing
      DelTxConstructing
    | -- | Transaction is sent to the wallet for signing
      DelTxSigning
    | -- | Transaction is sent to the backend for submission
      DelTxSubmitting
    | -- | Transaction is submitted to the blockchain
      DelTxSubmitted
    | DelTxError Text
    deriving (Eq)

textDelegateTxStatus :: DelegateTxStatus -> Text
textDelegateTxStatus = \case
    DelTxReady -> T.empty
    DelTxSuccess -> "Transaction finished successfully"
    DelTxConstructing -> "Constructing the transaction..."
    DelTxSigning -> "Please sign the transaction."
    DelTxSubmitting -> "Submitting..."
    DelTxSubmitted -> "Submitted. Pending the confirmation..."
    DelTxError e -> e

data VoteTxStatus
    = VoteTxReady
    | -- | Transaction is sent to the backend for constructing and balancing
      VoteTxConstructing
    | -- | Transaction is sent to the wallet for signing
      VoteTxSigning
    | -- | Transaction is sent to the backend for submission
      VoteTxSubmitting
    | -- | Transaction is submitted to the blockchain
      VoteTxSubmitted
    | VoteTxError Text
    deriving (Eq)

textVoteTxStatus :: VoteTxStatus -> Text
textVoteTxStatus = \case
    VoteTxReady -> T.empty
    VoteTxConstructing -> "Constructing the transaction..."
    VoteTxSigning -> "Please sign the transaction."
    VoteTxSubmitting -> "Submitting..."
    VoteTxSubmitted -> "Submitted. Pending the confirmation..."
    VoteTxError e -> "Error" <> column <> space <> e

-- Check if status is the performant one.
-- Performant status fires when background operations are processing.
isAppProcess :: AppStatus -> Bool
isAppProcess status =
    status
        `elem` [ WalletTx WalTxConstructing
               , WalletTx WalTxSigning
               , WalletTx WalTxSubmitting
               , WalletTx WalTxSubmitted
               , LedgerTx LedTxConstructing
               , LedgerTx LedTxSubmitted
               ]

isDelegateTxProcess :: DelegateTxStatus -> Bool
isDelegateTxProcess status =
    status `elem` [DelTxConstructing, DelTxSigning, DelTxSubmitting, DelTxSubmitted]

isVoteTxProcess :: VoteTxStatus -> Bool
isVoteTxProcess status =
    status
        `elem` [VoteTxConstructing, VoteTxSigning, VoteTxSubmitting, VoteTxSubmitted]

isAppTxProcessingBlock :: AppStatus -> Bool
isAppTxProcessingBlock = \case
    WalletTx WalTxConstructing -> True
    WalletTx WalTxSigning -> True
    WalletTx WalTxSubmitting -> True
    WalletTx WalTxSubmitted -> True
    WalletTx WalTxNoRelay -> True
    TransferTx TransTxConstructing -> True
    TransferTx TransTxSigning -> True
    TransferTx TransTxSubmitting -> True
    TransferTx TransTxSubmitted -> True
    TransferTx TransTxNoRelay -> True
    LedgerTx LedTxConstructing -> True
    LedgerTx LedTxSubmitted -> True
    LedgerTx LedTxNoRelay -> True
    LedgerTx LedTxInvalidChangeAddress -> True
    _ -> False

isAppNetworkBlock :: AppStatus -> Bool
isAppNetworkBlock = \case
    WalletInApp (WalletNetworkError _) -> True
    _ -> False

isAppTotalBlock :: AppStatus -> Bool
isAppTotalBlock s = isAppTxProcessingBlock s || isAppNetworkBlock s

isDaoTxProcessingBlock :: DaoStatus -> Bool
isDaoTxProcessingBlock = \case
    DelegateTx DelTxConstructing -> True
    DelegateTx DelTxSigning -> True
    DelegateTx DelTxSubmitting -> True
    DelegateTx DelTxSubmitted -> True
    VoteTx VoteTxConstructing -> True
    VoteTx VoteTxSigning -> True
    VoteTx VoteTxSubmitting -> True
    VoteTx VoteTxSubmitted -> True
    _ -> False

isDaoNetworkBlock :: DaoStatus -> Bool
isDaoNetworkBlock = \case
    WalletInDao (WalletNetworkError _) -> True
    _ -> False

isDaoTotalBlock :: DaoStatus -> Bool
isDaoTotalBlock s = isDaoTxProcessingBlock s || isDaoNetworkBlock s

relayError :: Text
relayError = "Relay returned an error!"

isAppReady :: AppStatus -> Bool
isAppReady status =
    status
        `elem` [ AppReady
               , WalletTx WalTxReady
               , TransferTx TransTxReady
               , LedgerTx LedTxReady
               , WalletInApp WalletReady
               , Migrate MigReady
               ]

isDaoReady :: DaoStatus -> Bool
isDaoReady status =
    status
        `elem` [ DaoReady
               , VoteTx VoteTxReady
               , DelegateTx DelTxReady
               , WalletInDao WalletReady
               ]

-- Used to hold status (processing status or with critical error) until Buffer statuses occur.
-- Buffer statuses are Ready, Tx Success, Tx WalletFail
-- After buffer are fired any status can be shown.
isDaoBuffer :: DaoStatus -> Bool
isDaoBuffer (DelegateTx DelTxReady) = True
isDaoBuffer (DelegateTx DelTxSuccess) = True
isDaoBuffer (VoteTx VoteTxReady) = True
isDaoBuffer (WalletInDao (WalletFail _)) = True
isDaoBuffer _ = False

isWalletError :: WalletStatus -> Bool
isWalletError (WalletFail _) = True
isWalletError _ = False

isAppStatusWantReload :: AppStatus -> Bool
isAppStatusWantReload (WalletTx WalTxNoRelay) = True
isAppStatusWantReload (TransferTx TransTxNoRelay) = True
isAppStatusWantReload (LedgerTx LedTxNoRelay) = True
isAppStatusWantReload _ = False

data UrlStatus
    = UrlEmpty
    | UrlInvalid
    | UrlValid
    deriving (Eq)

instance Show UrlStatus where
    show :: UrlStatus -> String
    show UrlEmpty = "URL is empty"
    show UrlInvalid = "Invalid URL format"
    show UrlValid = "Valid URL"

isNotValidUrl :: UrlStatus -> Bool
isNotValidUrl UrlEmpty = True
isNotValidUrl UrlInvalid = True
isNotValidUrl UrlValid = False

data CloudIconStatus = NoTokens | Saving | AllSaved | FailedSave
    deriving stock (Eq, Show)

isCloudIconStatus :: AppStatus -> Maybe CloudIconStatus
isCloudIconStatus (CloudIcon s) = Just s
isCloudIconStatus _ = Nothing

isTextAppStatus :: AppStatus -> Maybe AppStatus
isTextAppStatus (CloudIcon _) = Nothing
isTextAppStatus textStatus = Just textStatus
