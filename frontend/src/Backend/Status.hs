{-# LANGUAGE InstanceSigs #-}

module Backend.Status where

import Common.Utility (toText)
import qualified I18n.Common as I18n

import Data.Text (Text)


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

messageAppStatus :: AppStatus -> Either I18n.StatusMessage (I18n.StatusMessage, I18n.StatusMessage)
messageAppStatus appStatus =
    if isAppReady appStatus
        then Left I18n.SM_Empty
        else case appStatus of
            AppReady -> Left I18n.SM_Empty
            CustomStatus t -> Left $ I18n.SM_Custom t
            WalletTx s -> Right $ messageWalletTxStatus s
            TransferTx s -> Right $ messageTransferTxStatus s
            LedgerTx s -> Right $ messageLedgerTxStatus s
            CloudIcon s -> Right (I18n.SM_Cloud, I18n.SM_Custom $ toText s)
            CloudRestore s -> Right $ messageCloudRestoreStatus s
            Migrate s -> Right $ messageMigrateStatus s
            WalletInApp s -> Right $ messageWalletStatus s

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

messageWalletTxStatus :: WalletTxStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageWalletTxStatus wts = 
  let s = case wts of
        WalTxReady -> I18n.SM_Empty
        WalTxConstructing -> I18n.SM_Constructing
        WalTxSigning -> I18n.SM_Signing
        WalTxSubmitting -> I18n.SM_Submitting
        WalTxSubmitted -> I18n.SM_Submitted
        WalTxNoRelay -> I18n.SM_NoRelay
        WalTxBackendError e -> I18n.SM_Custom e
  in (I18n.SM_WalletMode, s)

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

messageTransferTxStatus :: TransferTxStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageTransferTxStatus tts = 
  let s = case tts of
        TransTxReady -> I18n.SM_Empty
        TransTxConstructing -> I18n.SM_Constructing
        TransTxSigning -> I18n.SM_Signing
        TransTxSubmitting -> I18n.SM_Submitting
        TransTxSubmitted -> I18n.SM_Submitted
        TransTxNoRelay -> I18n.SM_NoRelay
        TransTxBackendError e -> I18n.SM_Custom e
  in (I18n.SM_TransferMode, s)

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

messageLedgerTxStatus :: LedgerTxStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageLedgerTxStatus lts = 
  let s = case lts of 
        LedTxReady -> I18n.SM_Empty
        LedTxConstructing -> I18n.SM_Constructing
        LedTxSubmitted -> I18n.SM_Submitting
        LedTxNoRelay -> I18n.SM_NoRelay
        LedTxInvalidChangeAddress -> I18n.SM_InvalidChangeAddress
        LedTxBackendError e -> I18n.SM_Custom e
  in (I18n.SM_LedgerMode, s)

data WalletStatus
    = WalletReady
    | WalletNetworkError Text
    | WalletFail Text
    deriving (Eq)

messageWalletStatus :: WalletStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageWalletStatus ws = 
  let s = case ws of
        WalletReady -> I18n.SM_Empty
        WalletNetworkError t -> I18n.SM_Custom t
        WalletFail t -> I18n.SM_Custom t
  in (I18n.SM_WalletMode, s)
  

data MigrateStatus
    = MigReady
    | MigSuccess
    | MigUpdating
    deriving (Eq)

messageMigrateStatus :: MigrateStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageMigrateStatus ms = 
  let s = case ms of
        MigReady -> I18n.SM_Empty
        MigSuccess -> I18n.SM_SuccessMigration
        MigUpdating -> I18n.SM_MigrationUpdate
  in (I18n.SM_Migration, s)

data CloudRestoreStatus = RestoreFail | RestoreSuccess Int
    deriving stock (Eq)

messageCloudRestoreStatus :: CloudRestoreStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageCloudRestoreStatus crs = 
  let s = case crs of
        RestoreFail -> I18n.SM_RestoreFailed
        RestoreSuccess n -> I18n.SM_RestoreSuccess n
  in (I18n.SM_Cloud, s)

data DaoStatus
    = DaoReady
    | DelegateTx DelegateTxStatus
    | VoteTx VoteTxStatus
    | WalletInDao WalletStatus
    deriving stock (Eq)

messageDaoStatus :: DaoStatus -> Either I18n.StatusMessage (I18n.StatusMessage, I18n.StatusMessage)
messageDaoStatus daoStatus =
    if isDaoReady daoStatus
        then Left I18n.SM_Empty
        else case daoStatus of
            DaoReady -> Left I18n.SM_Empty
            DelegateTx s -> Right $ messageDelegateTxStatus s
            VoteTx s -> Right $ messageVoteTxStatus s
            WalletInDao s -> Right $ messageWalletStatus s

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

messageDelegateTxStatus :: DelegateTxStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageDelegateTxStatus dts = 
  let s = case dts of
        DelTxReady -> I18n.SM_Empty
        DelTxSuccess -> I18n.SM_TransactionSuccess
        DelTxConstructing -> I18n.SM_Constructing
        DelTxSigning -> I18n.SM_Signing
        DelTxSubmitting -> I18n.SM_Submitting
        DelTxSubmitted -> I18n.SM_Submitted
        DelTxError e -> I18n.SM_Custom e
  in (I18n.SM_Delegate, s)

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

messageVoteTxStatus :: VoteTxStatus -> (I18n.StatusMessage, I18n.StatusMessage)
messageVoteTxStatus vts = 
  let s = case vts of
        VoteTxReady -> I18n.SM_Empty
        VoteTxConstructing -> I18n.SM_Constructing
        VoteTxSigning -> I18n.SM_Signing
        VoteTxSubmitting -> I18n.SM_Submitting
        VoteTxSubmitted -> I18n.SM_Submitted
        VoteTxError e -> I18n.SM_Custom e
  in (I18n.SM_Vote, s)

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
