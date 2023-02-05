module Backend.Wallet where

import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Reflex.Dom                    hiding (Input)

import           Backend.Types
import           CSL                           (TransactionUnspentOutputs)
import           ENCOINS.Website.Widgets.Basic (image)
import           JS.App                        (walletLoad)
import           Widgets.Basic                 (elementResultJS)

data WalletName = None | Eternl | Nami | Flint | NuFi | Gero | Begin | Typhon
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

data Wallet = Wallet
  {
    walletName          :: WalletName,
    walletAddressBech32 :: Text,
    walletChangeAddress :: Address,
    walletUTXOs         :: TransactionUnspentOutputs
  }

loadWallet :: MonadWidget t m => Event t WalletName -> m (Dynamic t Wallet)
loadWallet eWalletName = mdo
  performEvent_ (walletLoad "" "" "" "changeAddressBech32Element" "pubKeyHashElement" "stakeKeyHashElement" "" "utxosElement" "" ""
    . toJS <$> eWalletName)
  dWalletName <- holdDyn None eWalletName
  dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
  dPubKeyHash <- elementResultJS "pubKeyHashElement" id
  dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
  dUTXOs <- elementResultJS "utxosElement" (fromMaybe [] . decodeText :: Text -> CSL.TransactionUnspentOutputs)
  let dAddrWallet =  zipDynWith mkAddressFromPubKeys dPubKeyHash dStakeKeyHash
  return $ Wallet <$> dWalletName <*> dWalletAddressBech32 <*> dAddrWallet <*> dUTXOs

walletIcon :: MonadWidget t m => WalletName -> m ()
walletIcon w = case w of
  None -> blank
  name -> image (toJS name <> ".svg") "wallet-image" "30px"