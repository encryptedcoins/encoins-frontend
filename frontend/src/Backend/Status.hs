{-# LANGUAGE InstanceSigs #-}
module Backend.Status where

import           Data.Text (Text, unpack)

data Status =
      Ready             -- ^ The default status
    | Constructing      -- ^ Transaction is sent to the backend for constructing and balancing
    | Signing           -- ^ Transaction is sent to the wallet for signing
    | Submitting        -- ^ Transaction is sent to the backend for submission
    | Submitted         -- ^ Transaction is submitted to the blockchain
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
    show Submitted              = "The transaction is now pending..."
    show (BackendError e)       = "Error: " <> unpack e
    show (WalletNetworkError e) = "Error: " <> unpack e
    show (WalletError e)        = "Error: " <> unpack e
    show (CustomStatus t)       = unpack t

-- Check if status is the performant one.
-- Performant status fires when background operations are processing.
isStatusBusy :: Status -> Bool
isStatusBusy status = status `elem` [Constructing, Signing, Submitting, Submitted]

isStatusBusyBackendNetwork :: Status -> Bool
isStatusBusyBackendNetwork = \case
  Constructing         -> True
  Signing              -> True
  Submitting           -> True
  Submitted            -> True
  BackendError _       -> True
  WalletNetworkError _ -> True
  _                    -> False

relayError :: Text
relayError = "All available relays are down! Try reloading the page or come back later."

isReady :: Status -> Bool
isReady Ready = True
isReady _     = False

isBlockError :: Status -> Bool
isBlockError (BackendError _) = True
isBlockError _                = False

data UrlStatus
    = UrlEmpty
    | UrlInvalid
    | UrlValid
    -- | UrlPingFail
    -- | UrlPingSuccess
    deriving Eq

instance Show UrlStatus where
    show :: UrlStatus -> String
    show UrlEmpty       = "URL is empty"
    show UrlInvalid     = "Format of URL is invalid"
    show UrlValid       = "Format of URL is valid"
    -- show UrlPingFail    = "Ping of relay is failed"
    -- show UrlPingSuccess = "Ping of relay is succeeded"

isNotValidUrl :: UrlStatus -> Bool
isNotValidUrl UrlEmpty       = True
isNotValidUrl UrlInvalid     = True
isNotValidUrl UrlValid       = False
-- isNotValidUrl UrlPingFail    = True
-- isNotValidUrl UrlPingSuccess = False
