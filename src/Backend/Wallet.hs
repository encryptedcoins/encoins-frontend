module Backend.Wallet where

import           Control.Monad                 (void)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Reflex.Dom                    hiding (Input)

import           Backend.Protocol.Types
import           CSL                           (TransactionUnspentOutputs)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.Common.Widgets.Basic  (image)
import           JS.App                        (walletLoad)

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
  _            -> None

walletsSupportedInApp :: [WalletName]
walletsSupportedInApp = [Eternl, Flint, Nami, None]

walletsSupportedInDAO :: [WalletName]
walletsSupportedInDAO = [Eternl, Flint, Gero, Nami, NuFi, None]

data Wallet = Wallet
  {
    walletName              :: WalletName,
    walletNetworkId         :: Text,
    walletAddressBech32     :: Text,
    walletChangeAddress     :: Address,
    walletUTXOs             :: TransactionUnspentOutputs
  }
  deriving (Show, Eq)

loadWallet :: MonadWidget t m => Event t WalletName -> m (Dynamic t Wallet)
loadWallet eWalletName = mdo
  performEvent_ (walletLoad . toJS <$> eWalletName)
  dWalletName <- elementResultJS "walletNameElement" fromJS
  dWalletNetworkId <- elementResultJS "networkIdElement" id
  dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
  dPubKeyHash <- elementResultJS "pubKeyHashElement" id
  dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
  dUTXOs <- elementResultJS "utxosElement" (fromMaybe [] . decodeText :: Text -> CSL.TransactionUnspentOutputs)
  let dAddrWallet = zipDynWith mkAddressFromPubKeys dPubKeyHash (checkEmptyText <$> dStakeKeyHash)
  return $ Wallet <$> dWalletName <*> dWalletNetworkId <*> dWalletAddressBech32 <*> dAddrWallet <*> dUTXOs

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
