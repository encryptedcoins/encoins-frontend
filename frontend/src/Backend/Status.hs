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
    | -- | Transaction is passed successfully
      WalTxSuccess
    | -- | Transaction is sent to the backend for constructing and balancing
      WalTxConstructing
    | -- | Transaction is sent to the wallet for signing
      WalTxSigning
    | -- | Transaction is sent to the backend for submission
      WalTxSubmitting
    | -- | Transaction is submitted to the blockchain
      WalTxSubmitted
    | -- | All errors are gone
      WalTxNoError
    | -- | All relay are down. Need reload
      WalTxNoRelay
    | WalTxBackendError Text
    deriving (Eq)

textWalletTxStatus :: WalletTxStatus -> Text
textWalletTxStatus = \case
    WalTxReady -> T.empty
    WalTxSuccess -> "Transaction finished successfully"
    WalTxConstructing -> "Constructing the transaction..."
    WalTxSigning -> "Please sign the transaction."
    WalTxSubmitting -> "Submitting..."
    WalTxSubmitted -> "Submitted. Pending the confirmation..."
    WalTxNoRelay -> "All available relays are down!"
    WalTxNoError -> T.empty
    WalTxBackendError e -> e

data TransferTxStatus
    = -- Default, initial status
      TransTxReady
    | -- | Transaction is passed successfully
      TransTxSuccess
    | -- | Transaction is sent to the backend for constructing and balancing
      TransTxConstructing
    | -- | Transaction is sent to the wallet for signing
      TransTxSigning
    | -- | Transaction is sent to the backend for submission
      TransTxSubmitting
    | -- | Transaction is submitted to the blockchain
      TransTxSubmitted
    | -- | All errors are gone
      TransTxNoError
    | -- | All relay are down. Need reload
      TransTxNoRelay
    | TransTxBackendError Text
    deriving (Eq)

textTransferTxStatus :: TransferTxStatus -> Text
textTransferTxStatus = \case
    TransTxReady -> T.empty
    TransTxSuccess -> "Transaction finished successfully"
    TransTxConstructing -> "Constructing the transaction..."
    TransTxSigning -> "Please sign the transaction."
    TransTxSubmitting -> "Submitting..."
    TransTxSubmitted -> "Submitted. Pending the confirmation..."
    TransTxNoRelay -> "All available relays are down!"
    TransTxNoError -> T.empty
    TransTxBackendError e -> "Error" <> column <> space <> e

data LedgerTxStatus
    = -- Default, initial status
      LedTxReady
    | -- | Transaction is passed successfully
      LedTxSuccess
    | -- | Transaction is sent to the backend for constructing and balancing
      LedTxConstructing
    | -- | Transaction is sent to the wallet for signing
      LedTxSigning
    | -- | Transaction is sent to the backend for submission
      LedTxSubmitting
    | -- | Transaction is submitted to the blockchain
      LedTxSubmitted
    | -- | All errors are gone
      LedTxNoError
    | -- | All relay are down. Need reload
      LedTxNoRelay
    | -- | Change address is invalid in Ledger mode
      LedTxInvalidChangeAddress
    | LedTxBackendError Text
    deriving (Eq)

textLedgerTxStatus :: LedgerTxStatus -> Text
textLedgerTxStatus = \case
    LedTxReady -> T.empty
    LedTxSuccess -> "Transaction finished successfully"
    LedTxConstructing -> "Constructing the transaction..."
    LedTxSigning -> "Please sign the transaction."
    LedTxSubmitting -> "Submitting..."
    LedTxSubmitted -> "Submitted. Pending the confirmation..."
    LedTxNoRelay -> "All available relays are down!"
    LedTxInvalidChangeAddress -> "ChangeAddress is invalid"
    LedTxNoError -> T.empty
    LedTxBackendError e -> "Error" <> column <> space <> e

data WalletStatus
    = WalletReady
    | WalletNetworkError Text
    | WalletError Text
    deriving (Eq)

textWalletStatus :: WalletStatus -> Text
textWalletStatus = \case
    WalletReady -> T.empty
    WalletNetworkError t -> t
    WalletError t -> t

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
    | -- | All errors are gone
      DelTxNoError
    | -- | All relay are down. Need reload
      DelTxNoRelay
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
    DelTxNoRelay -> "All available relays are down!"
    DelTxNoError -> T.empty
    DelTxError e -> e

data VoteTxStatus
    = VoteTxReady
    | -- | Transaction is passed successfully
      VoteTxSuccess
    | -- | Transaction is sent to the backend for constructing and balancing
      VoteTxConstructing
    | -- | Transaction is sent to the wallet for signing
      VoteTxSigning
    | -- | Transaction is sent to the backend for submission
      VoteTxSubmitting
    | -- | Transaction is submitted to the blockchain
      VoteTxSubmitted
    | -- | All errors are gone
      VoteTxNoError
    | -- | All relay are down. Need reload
      VoteTxNoRelay
    | VoteTxError Text
    deriving (Eq)

textVoteTxStatus :: VoteTxStatus -> Text
textVoteTxStatus = \case
    VoteTxReady -> T.empty
    VoteTxSuccess -> "Transaction finished successfully"
    VoteTxConstructing -> "Constructing the transaction..."
    VoteTxSigning -> "Please sign the transaction."
    VoteTxSubmitting -> "Submitting..."
    VoteTxSubmitted -> "Submitted. Pending the confirmation..."
    VoteTxNoRelay -> "All available relays are down!"
    VoteTxNoError -> T.empty
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
               , LedgerTx LedTxSigning
               , LedgerTx LedTxSubmitting
               , LedgerTx LedTxSubmitted
               ]

isDelegateTxProcess :: DelegateTxStatus -> Bool
isDelegateTxProcess status =
    status `elem` [DelTxConstructing, DelTxSigning, DelTxSubmitting, DelTxSubmitted]

isVoteTxProcess :: VoteTxStatus -> Bool
isVoteTxProcess status =
    status
        `elem` [VoteTxConstructing, VoteTxSigning, VoteTxSubmitting, VoteTxSubmitted]

isAppStatusWantBlockButtons :: AppStatus -> Bool
isAppStatusWantBlockButtons = \case
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
    LedgerTx LedTxSigning -> True
    LedgerTx LedTxSubmitting -> True
    LedgerTx LedTxSubmitted -> True
    LedgerTx LedTxNoRelay -> True
    LedgerTx LedTxInvalidChangeAddress -> True
    WalletInApp (WalletNetworkError _) -> True
    _ -> False

isDaoStatusWantBlockButtons :: DaoStatus -> Bool
isDaoStatusWantBlockButtons = \case
    DelegateTx DelTxConstructing -> True
    DelegateTx DelTxSigning -> True
    DelegateTx DelTxSubmitting -> True
    DelegateTx DelTxSubmitted -> True
    DelegateTx DelTxNoRelay -> True
    VoteTx VoteTxConstructing -> True
    VoteTx VoteTxSigning -> True
    VoteTx VoteTxSubmitting -> True
    VoteTx VoteTxSubmitted -> True
    VoteTx VoteTxNoRelay -> True
    WalletInDao (WalletNetworkError _) -> True
    _ -> False

relayError :: Text
relayError = "Relay returned an error!"

isAppReady :: AppStatus -> Bool
isAppReady status =
    status
        `elem` [ AppReady
               , WalletTx WalTxReady
               , WalletTx WalTxNoError
               , TransferTx TransTxReady
               , TransferTx TransTxNoError
               , LedgerTx LedTxReady
               , LedgerTx LedTxNoError
               , WalletInApp WalletReady
               , Migrate MigReady
               ]

isDaoReady :: DaoStatus -> Bool
isDaoReady status =
    status
        `elem` [ DaoReady
               , VoteTx VoteTxReady
               , VoteTx VoteTxNoError
               , DelegateTx DelTxReady
               , DelegateTx DelTxNoError
               , WalletInDao WalletReady
               ]

-- Used to hold status (processing status or with critical error) until Buffer statuses occur.
-- Buffer statuses are Ready, Tx Success, Tx WalletError
-- After buffer are fired any status can be shown.
isDaoBuffer :: DaoStatus -> Bool
isDaoBuffer (DelegateTx DelTxReady) = True
isDaoBuffer (DelegateTx DelTxSuccess) = True
isDaoBuffer (VoteTx VoteTxReady) = True
isDaoBuffer (VoteTx VoteTxSuccess) = True
isDaoBuffer (WalletInDao (WalletError _)) = True
isDaoBuffer _ = False

isDaoReadyOrNoError :: DaoStatus -> Bool
isDaoReadyOrNoError (DelegateTx DelTxReady) = True
isDaoReadyOrNoError (DelegateTx DelTxNoError) = True
isDaoReadyOrNoError (VoteTx VoteTxReady) = True
isDaoReadyOrNoError (VoteTx VoteTxNoError) = True
isDaoReadyOrNoError _ = False

isWalletError :: WalletStatus -> Bool
isWalletError (WalletError _) = True
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
