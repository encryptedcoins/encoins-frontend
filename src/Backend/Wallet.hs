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

data WalletName =
    Eternl
  | Nami
  -- | Flint
  -- | NuFi
  -- | Gero
  -- | Begin
  -- | Typhon
  -- | Lace
  | None
  deriving (Eq, Show, Enum, Bounded)

toJS :: WalletName -> Text
toJS = \case
  None   -> "none"
  Eternl -> "eternl"
  Nami   -> "nami"
  -- Flint  -> "flint"
  -- NuFi   -> "nufi"
  -- Gero   -> "gerowallet"
  -- Begin  -> "begin"
  -- Begin  -> "begin-nightly"
  -- Typhon -> "typhon"
  -- Lace   -> "lace"

fromJS :: Text -> WalletName
fromJS = \case
  "eternl"     -> Eternl
  "nami"       -> Nami
  -- "flint"      -> Flint
  -- "nufi"       -> NuFi
  -- "gerowallet" -> Gero
  -- "begin"      -> Begin
  -- "begin-nightly" -> Begin
  -- "typhon"     -> Typhon
  -- "lace"       -> Lace
  _            -> None

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
