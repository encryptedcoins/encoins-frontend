module Backend.Wallet where

import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Reflex.Dom                    hiding (Input)

import           Backend.Types
import           CSL                           (TransactionUnspentOutputs)
import           ENCOINS.Website.Widgets.Basic (image)
import           JS.App                        (walletLoad)
import           Widgets.Basic                 (elementResultJS)

data WalletName = None | Eternl | Nami | Flint | NuFi | Gero | Begin | Typhon | Lace
  deriving (Eq, Show)

toJS :: WalletName -> Text
toJS = \case
  None   -> "none"
  Eternl -> "eternl"
  Nami   -> "nami"
  Flint  -> "flint"
  NuFi   -> "nufi"
  Gero   -> "gerowallet"
  Begin  -> "begin"
  Typhon -> "typhon"
  Lace   -> "lace"

fromJS :: Text -> WalletName
fromJS = \case
  "eternl"     -> Eternl
  "nami"       -> Nami
  "flint"      -> Flint
  "nufi"       -> NuFi
  "gerowallet" -> Gero
  "begin"      -> Begin
  "typhon"     -> Typhon
  "lace"       -> Lace
  _            -> None

data Wallet = Wallet
  {
    walletName          :: WalletName,
    walletAddressBech32 :: Text,
    walletChangeAddress :: Address,
    walletUTXOs         :: TransactionUnspentOutputs
  }
  deriving (Show, Eq)

loadWallet :: MonadWidget t m => Event t WalletName -> m (Dynamic t Wallet)
loadWallet eWalletName = mdo
  performEvent_ (walletLoad . toJS <$> eWalletName)
  dWalletName <- elementResultJS "walletNameElement" fromJS
  dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
  dPubKeyHash <- elementResultJS "pubKeyHashElement" id
  dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
  dUTXOs <- elementResultJS "utxosElement" (fromMaybe [] . decodeText :: Text -> CSL.TransactionUnspentOutputs)
  let dAddrWallet = zipDynWith mkAddressFromPubKeys dPubKeyHash dStakeKeyHash
  return $ Wallet <$> dWalletName <*> dWalletAddressBech32 <*> dAddrWallet <*> dUTXOs

walletIcon :: MonadWidget t m => WalletName -> m ()
walletIcon w = case w of
  None -> blank
  name -> image (toJS name <> ".svg") "wallet-image" "30px"

data WalletError = WalletError
  {
    walletErrorName :: WalletName,
    walletErrorText :: Text
  }
  deriving (Show, Eq)