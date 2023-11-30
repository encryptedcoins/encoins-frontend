{-# LANGUAGE DeriveAnyClass #-}
module Backend.Wallet where

import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)

import           Backend.Protocol.Types
import           Backend.Utility        (isMultiAssetOf)
import           Config.Config          (NetworkConfig (..), NetworkId (..),
                                         networkConfig)
import           CSL                    (TransactionUnspentOutputs)
import qualified CSL

data WalletName
  =
    -- Begin
    Eternl
  | Flint
  | Gero
  -- | Lace
  | Nami
  | NuFi
  -- | Typhon
  | Yoroi
  | None
  deriving (Eq, Show, Enum, Bounded)

toJS :: WalletName -> Text
toJS = \case
  -- Begin  -> "begin"
  -- Begin  -> "begin-nightly"
  Eternl -> "eternl"
  Flint  -> "flint"
  Gero   -> "gerowallet"
  -- Lace   -> "lace"
  Nami   -> "nami"
  NuFi   -> "nufi"
  -- Typhon -> "typhon"
  Yoroi  -> "yoroi"
  None   -> "none"

fromJS :: Text -> WalletName
fromJS = \case
  -- "begin"      -> Begin
  -- "begin-nightly" -> Begin
  "eternl"     -> Eternl
  "flint"      -> Flint
  "gerowallet" -> Gero
  -- "lace"       -> Lace
  "nami"       -> Nami
  "nufi"       -> NuFi
  -- "typhon"     -> Typhon
  "yoroi"      -> Yoroi
  _            -> None

walletsSupportedInAppTestnet :: [WalletName]
walletsSupportedInAppTestnet = [Eternl, Flint, Nami, None]

-- TODO add more
walletsSupportedInAppMainnet :: [WalletName]
walletsSupportedInAppMainnet = [Eternl, Flint, Nami, None]

walletsSupportedInDAOMainnet :: [WalletName]
walletsSupportedInDAOMainnet = [Eternl, Flint, Gero, Nami, NuFi, Yoroi, None]

-- TODO checkup list. Add or remove other
walletsSupportedInDAOTestnet :: [WalletName]
walletsSupportedInDAOTestnet = [Eternl, Flint, Nami, None]

walletsSupportedInDAO :: [WalletName]
walletsSupportedInDAO = case dao networkConfig of
  Mainnet -> walletsSupportedInDAOMainnet
  Testnet -> walletsSupportedInDAOTestnet

walletsSupportedInApp :: [WalletName]
walletsSupportedInApp = case app networkConfig of
  Mainnet -> walletsSupportedInAppMainnet
  Testnet -> walletsSupportedInAppTestnet

data Wallet = Wallet
  {
    walletName          :: WalletName,
    walletNetworkId     :: NetworkId,
    walletAddressBech32 :: Text,
    walletChangeAddress :: Address,
    walletUTXOs         :: TransactionUnspentOutputs
  }
  deriving (Show, Eq)

data WalletError = WalletError
  {
    walletErrorName :: WalletName,
    walletErrorText :: Text
  }
  deriving (Show, Eq)

data LucidConfig = LucidConfig
  { apiKey       :: Text
  , networkId    :: Text
  , encPolicyId  :: Text
  , encAssetName :: Text
  }

-- (apiKey of blockfrost, networkId, Enc PolicyId, Enc AssetName)
lucidConfigDao :: LucidConfig
lucidConfigDao = case dao networkConfig of
  Mainnet -> LucidConfig
    { apiKey = "mainnetK4sRBCTDwqzK1KRuFxnpuxPbKF4ZQrnl"
    , networkId = "Mainnet"
    , encPolicyId = "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96"
    , encAssetName = "454e4353"
    }
  Testnet -> LucidConfig
    { apiKey = "preprodCMZ4wTbLIsLRncviikOkicVgYXyfYrga"
    , networkId = "Preprod"
    , encPolicyId = "2912c4707b5695db33e60e89467125bda41fecd62c7f8e56cd854247"
    , encAssetName = "454e4353"
    }

currentNetworkApp :: Text
currentNetworkApp = case app networkConfig of
  Mainnet -> "Mainnet"
  Testnet -> "Testnet Preprod"

hasToken :: Text -> Text -> Wallet -> Bool
hasToken symbol token = any
  ( fromMaybe False
  . fmap (isMultiAssetOf symbol token)
  . CSL.multiasset
  . CSL.amount
  . CSL.output
  )
  . walletUTXOs
