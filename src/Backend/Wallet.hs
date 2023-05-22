module Backend.Wallet where

import           Control.Monad                 (void)
import           Data.Maybe                    (fromMaybe, fromJust)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           PlutusTx.Builtins
import           Reflex.Dom                    hiding (Input)
import           Text.Hex                      (decodeHex, encodeHex)

import           Backend.Types
import           CSL                           (TransactionUnspentOutputs)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Common.Widgets.Basic  (image)
import           JS.App                        (sha2_256, walletLoad)

data WalletName =
    Eternl
  | Nami
  | Flint
  -- | NuFi
  -- | Gero
  -- | Begin
  -- | Typhon
  | Lace
  | None
  deriving (Eq, Show, Enum, Bounded)

toJS :: WalletName -> Text
toJS = \case
  None   -> "none"
  Eternl -> "eternl"
  Nami   -> "nami"
  Flint  -> "flint"
  -- NuFi   -> "nufi"
  -- Gero   -> "gerowallet"
  -- Begin  -> "begin"
  -- Begin  -> "begin-nightly"
  -- Typhon -> "typhon"
  Lace   -> "lace"

fromJS :: Text -> WalletName
fromJS = \case
  "eternl"     -> Eternl
  "nami"       -> Nami
  "flint"      -> Flint
  -- "nufi"       -> NuFi
  -- "gerowallet" -> Gero
  -- "begin"      -> Begin
  -- "begin-nightly" -> Begin
  -- "typhon"     -> Typhon
  "lace"       -> Lace
  _            -> None

data Wallet = Wallet
  {
    walletName              :: WalletName,
    walletNetworkId         :: Text,
    walletAddressBech32     :: Text,
    walletChangeAddress     :: Address,
    walletUTXOs             :: TransactionUnspentOutputs,
    walletBulletproofParams :: GroupElement
  }
  deriving (Show, Eq)

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "58a04846566cd531318ee2e98e3044647f6c75e8224396515cc8aee9")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")

addressToBytes :: Address -> Text
addressToBytes (Address cr scr) = bs1 `Text.append` bs2
    where
        bs1 = encodeHex $ fromBuiltin $ case cr of
          PubKeyCredential (PubKeyHash pkh) -> pkh
          ScriptCredential (ValidatorHash vh) -> vh
        bs2 = encodeHex $ fromBuiltin $ case scr of
          Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> pkh
          Just (StakingHash (ScriptCredential (ValidatorHash vh))) -> vh
          _       -> emptyByteString

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
  -- Obtaining BulletproofParams
  dBulletproofParams <- elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)
  performEvent_ (flip sha2_256 "bulletproofParamsElement" . Text.append (addressToBytes ledgerAddress) . addressToBytes <$> updated dAddrWallet)
  return $ Wallet <$> dWalletName <*> dWalletNetworkId <*> dWalletAddressBech32
    <*> dAddrWallet <*> dUTXOs <*> dBulletproofParams

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
