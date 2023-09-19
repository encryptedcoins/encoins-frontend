{-# LANGUAGE DeriveAnyClass #-}
module Backend.Wallet where

import           Control.Monad                 (void)
import           Data.Maybe                    (fromMaybe, fromJust)
import           Data.Text                     (Text)
import           Data.ByteString.Lazy          (fromStrict)
import           Reflex.Dom                    hiding (Input)

import           Backend.Protocol.Types
import           CSL                           (TransactionUnspentOutputs)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.Common.Widgets.Basic  (image)
import           JS.App                        (walletLoad)
import           Data.Aeson (FromJSON, decode)
import           GHC.Generics (Generic)
import           Data.FileEmbed                  (embedFile)
import qualified Data.Text as T
import ENCOINS.Common.Events
import Debug.Trace

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
    walletName              :: WalletName,
    walletNetworkId         :: NetworkId,
    walletAddressBech32     :: Text,
    walletChangeAddress     :: Address,
    walletUTXOs             :: TransactionUnspentOutputs
  }
  deriving (Show, Eq)

loadWallet :: MonadWidget t m => Event t WalletName -> m (Dynamic t Wallet)
loadWallet eWalletName = mdo
  performEvent_ (walletLoad . toJS <$> eWalletName)
  dWalletName <- elementResultJS "walletNameElement" fromJS
  eWalletNetworkId <- updated <$> elementResultJS "networkIdElement" id
  dWalletNetworkId <- foldDynMaybe
    (\n _ -> if T.null n then Nothing else Just $ toNetworkId n)
    Testnet
    eWalletNetworkId
  dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
  dPubKeyHash <- elementResultJS "pubKeyHashElement" id
  dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
  dUTXOs <- elementResultJS "utxosElement" (fromMaybe [] . decodeText :: Text -> CSL.TransactionUnspentOutputs)
  let dAddrWallet = zipDynWith mkAddressFromPubKeys dPubKeyHash (checkEmptyText <$> dStakeKeyHash)
  return $ Wallet
    <$> dWalletName
    <*> dWalletNetworkId
    <*> dWalletAddressBech32
    <*> dAddrWallet
    <*> dUTXOs

walletIcon :: MonadWidget t m => WalletName -> m ()
walletIcon w = case w of
  None -> blank
  name -> void $ image (pure $ toJS name <> ".svg") "wallet-image" "30px"

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
  fromJust $ decode $ fromStrict $(embedFile "config/network_id_config.json")

lucidConfig :: (Text, Text)
lucidConfig = case dao networkConfig of
  Mainnet -> ("mainnetK4sRBCTDwqzK1KRuFxnpuxPbKF4ZQrnl", "Mainnet")
  Testnet -> ("preprodCMZ4wTbLIsLRncviikOkicVgYXyfYrga", "Preprod")