{-# LANGUAGE InstanceSigs #-}
module Backend.Status where

import           Data.Text (Text, unpack)

data Status =
      Ready                -- ^ The default status
    | Success              -- ^ Transaction is passed successfully
    | Constructing         -- ^ Transaction is sent to the backend for constructing and balancing
    | Signing              -- ^ Transaction is sent to the wallet for signing
    | Submitting           -- ^ Transaction is sent to the backend for submission
    | Submitted            -- ^ Transaction is submitted to the blockchain
    | NoError              -- ^ All errors are gone
    | NoRelay              -- ^ All relay are down. Need reload
    | CacheMigrated        -- ^ Cache is migrated to new version. Reload wanted.
    | InvalidChangeAddress -- ^ Change address is invalid in Ledger mode
    | BackendError Text
    | WalletNetworkError Text
    | WalletError Text
    | CustomStatus Text
    deriving Eq

instance Show Status where
    show Ready                  = ""
    show Success                = "Transaction finished successfully"
    show Constructing           = "Constructing the transaction..."
    show Signing                = "Please sign the transaction."
    show Submitting             = "Submitting..."
    show Submitted              = "Submitted. Pending the confirmation..."
    show NoRelay                = "All available relays are down!"
    show CacheMigrated          = "Local cache was updated to last version"
    show InvalidChangeAddress   = "ChangeAddress is invalid"
    show NoError                = ""
    show (BackendError e)       = "Error: " <> unpack e
    show (WalletNetworkError e) = "Error: " <> unpack e
    show (WalletError e)        = "Error: " <> unpack e
    show (CustomStatus t)       = unpack t

-- Check if status is the performant one.
-- Performant status fires when background operations are processing.
isTxProcess :: Status -> Bool
isTxProcess status = status `elem` [Constructing, Signing, Submitting, Submitted]

isStatusWantBlockButtons :: Status -> Bool
isStatusWantBlockButtons = \case
  Constructing         -> True
  Signing              -> True
  Submitting           -> True
  Submitted            -> True
  NoRelay              -> True
  InvalidChangeAddress -> True
  WalletNetworkError _ -> True
  _                    -> False

relayError :: Text
relayError = "Relay returned an error!"

isReady :: Status -> Bool
isReady Ready = True
isReady _     = False

-- Used to hold status (processing status or with critical error)
-- until Buffer statuses occur.
-- After they fired any status can be shown.
isBuffer :: Status -> Bool
isBuffer Ready           = True
isBuffer Success         = True
isBuffer (WalletError _) = True
isBuffer _               = False

isReadyOrNoError :: Status -> Bool
isReadyOrNoError Ready   = True
isReadyOrNoError NoError = True
isReadyOrNoError _       = False

isWalletError :: Status -> Bool
isWalletError (WalletError _) = True
isWalletError _               = False

isStatusWantReload :: Status -> Bool
isStatusWantReload NoRelay       = True
isStatusWantReload _             = False

data UrlStatus
    = UrlEmpty
    | UrlInvalid
    | UrlValid
    deriving Eq

instance Show UrlStatus where
    show :: UrlStatus -> String
    show UrlEmpty   = "URL is empty"
    show UrlInvalid = "Invalid URL format"
    show UrlValid   = "Valid URL"

isNotValidUrl :: UrlStatus -> Bool
isNotValidUrl UrlEmpty   = True
isNotValidUrl UrlInvalid = True
isNotValidUrl UrlValid   = False

data SaveIconStatus = NoTokens | Saving | AllSaved | FailedSave
  deriving stock (Eq, Show)

data AppStatus = Save SaveIconStatus | Tx (Text, Status)
  deriving stock (Eq, Show)

isSaveStatus :: AppStatus -> Maybe SaveIconStatus
isSaveStatus (Save s) = Just s
isSaveStatus _        = Nothing

isStatus :: AppStatus -> Maybe (Text, Status)
isStatus (Tx s) = Just s
isStatus _      = Nothing
