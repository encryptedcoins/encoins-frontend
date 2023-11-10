{-# LANGUAGE DeriveAnyClass #-}
module Backend.Wallet where

import           Data.Aeson             (FromJSON, decode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           Backend.Protocol.Types
import           Config.Config          (networkConfigBS)
import           CSL                    (TransactionUnspentOutputs)

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

data NetworkId = Testnet | Mainnet
  deriving (Eq, Show, Generic, FromJSON, Enum)

toNetworkId :: Text -> NetworkId
toNetworkId = toEnum . read @Int . T.unpack

data NetworkConfig = NetworkConfig
  { dao :: NetworkId
  , app :: NetworkId
  }
  deriving (Eq, Show, Generic, FromJSON)

networkConfig :: NetworkConfig
networkConfig =
  fromJust $ decode $ fromStrict networkConfigBS

lucidConfigDao :: (Text, Text)
lucidConfigDao = case dao networkConfig of
  Mainnet -> ("mainnetK4sRBCTDwqzK1KRuFxnpuxPbKF4ZQrnl", "Mainnet")
  Testnet -> ("preprodCMZ4wTbLIsLRncviikOkicVgYXyfYrga", "Preprod")

currentNetworkApp :: Text
currentNetworkApp = case app networkConfig of
  Mainnet -> "Mainnet"
  Testnet -> "Testnet Preprod"
