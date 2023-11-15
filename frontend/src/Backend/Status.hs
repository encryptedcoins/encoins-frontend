{-# LANGUAGE InstanceSigs #-}
module Backend.Status where

import           Data.Text (Text, unpack)

data Status =
      Ready             -- ^ The default status
    | Constructing      -- ^ Transaction is sent to the backend for constructing and balancing
    | Signing           -- ^ Transaction is sent to the wallet for signing
    | Submitting        -- ^ Transaction is sent to the backend for submission
    | Submitted         -- ^ Transaction is submitted to the blockchain
    | NoRelay           -- ^ All relay are down
    | BackendError Text
    | WalletNetworkError Text
    | WalletError Text
    | CustomStatus Text
    deriving Eq

instance Show Status where
    show Ready                  = ""
    show Constructing           = "Constructing the transaction..."
    show Signing                = "Please sign the transaction."
    show Submitting             = "Submitting..."
    show Submitted              = "Submitted. Pending the confirmation..."
    show NoRelay                = "All available relay are down!"
    show (BackendError e)       = "Error: " <> unpack e
    show (WalletNetworkError e) = "Error: " <> unpack e
    show (WalletError e)        = "Error: " <> unpack e
    show (CustomStatus t)       = unpack t

-- Check if status is the performant one.
-- Performant status fires when background operations are processing.
isTxProcess :: Status -> Bool
isTxProcess status = status `elem` [Constructing, Signing, Submitting, Submitted]

isTxProcessOrCriticalError :: Status -> Bool
isTxProcessOrCriticalError = \case
  Constructing         -> True
  Signing              -> True
  Submitting           -> True
  Submitted            -> True
  NoRelay              -> True
  WalletNetworkError _ -> True
  _                    -> False

relayError :: Text
relayError = "Relay returned an error!"

isReady :: Status -> Bool
isReady Ready = True
isReady _     = False

isWalletError :: Status -> Bool
isWalletError (WalletError _) = True
isWalletError _               = False

isNoRelay :: Status -> Bool
isNoRelay NoRelay = True
isNoRelay _       = False

data UrlStatus
    = UrlEmpty
    | UrlInvalid
    | UrlValid
    -- | UrlPingFail
    -- | UrlPingSuccess
    deriving Eq

instance Show UrlStatus where
    show :: UrlStatus -> String
    show UrlEmpty   = "URL is empty"
    show UrlInvalid = "Invalid URL format"
    show UrlValid   = "Valid URL"
    -- show UrlPingFail    = "Relay is not found"
    -- show UrlPingSuccess = "Found relay"

isNotValidUrl :: UrlStatus -> Bool
isNotValidUrl UrlEmpty   = True
isNotValidUrl UrlInvalid = True
isNotValidUrl UrlValid   = False
-- isNotValidUrl UrlPingFail    = True
-- isNotValidUrl UrlPingSuccess = False
