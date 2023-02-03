module Backend.Wallet where

import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Reflex.Dom                  hiding (Input)

import           Backend.Types
import           CSL                         (TransactionUnspentOutputs)
import           JS.App                      (walletLoad)
import           Widgets.Basic               (elementResultJS)
import           Widgets.Events              (newEvent)

data Wallet = None | Eternl | Nami | Flint | NuFi | Gero | Begin | Typhon
  deriving (Eq, Show)

toJS :: Wallet -> Text
toJS = \case
  None   -> "none"
  Eternl -> "eternl"
  Nami   -> "nami"
  Flint  -> "flint"
  NuFi   -> "nufi"
  Gero   -> "gerowallet"
  Begin  -> "begin"
  Typhon -> "typhon"

loadWallet :: MonadWidget t m => Wallet -> m (Dynamic t Address, Dynamic t TransactionUnspentOutputs)
loadWallet w = mdo
  e <- newEvent
  dPubKeyHash <- elementResultJS "pubKeyHashElement" id
  dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
  dUTXOs <- elementResultJS "utxosElement" (fromMaybe [] . decodeText :: Text -> CSL.TransactionUnspentOutputs)
  let dAddrWallet =  zipDynWith mkAddressFromPubKeys dPubKeyHash dStakeKeyHash
  performEvent_ (walletLoad (toJS w) "" "" "" "" "pubKeyHashElement" "stakeKeyHashElement" "" "utxosElement" "" "" <$ e)
  return (dAddrWallet, dUTXOs)