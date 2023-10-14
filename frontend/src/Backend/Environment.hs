module Backend.Environment where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Maybe                (fromJust, fromMaybe)
import qualified Data.Text                 as Text
import           GHC.Real                  (fromIntegral)
import           PlutusTx.Builtins
import           Reflex.Dom                hiding (Input)
import           System.Random             (randomIO)
import           Text.Hex                  (decodeHex, encodeHex)

import           Backend.Protocol.Fees     (protocolFees)
import           Backend.Protocol.Setup    (ledgerAddress)
import           Backend.Protocol.Types
import           Backend.Wallet            (Wallet (..))
import           ENCOINS.App.Widgets.Basic (elementResultJS)
import           ENCOINS.Bulletproofs
import           ENCOINS.Crypto.Field      (Field (..), fromFieldElement)
import           JS.App                    (sha2_256)
import           PlutusTx.Extra.ByteString (ToBuiltinByteString (..))


getBalance :: MonadWidget t m
  => EncoinsMode
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> m (Dynamic t Integer)
getBalance mode dCoinsBurn dCoinsMint = do
  let getAda = fmap (sum . map (fromFieldElement . secretV))
  let getDeposit = fmap ((*4) . fromIntegral . length)
  case mode of
    WalletMode -> holdUniqDyn $ zipDynWith (-) (getAda dCoinsBurn) (getAda dCoinsMint)
    -- In Transfer mode
    -- It count deposit amount that should be put
    TransferMode -> holdUniqDyn $ getDeposit dCoinsBurn
    LedgerMode -> do
      dEncoinsBalance <- holdUniqDyn $ zipDynWith (-) (getAda dCoinsBurn) (getAda dCoinsMint)
      dDepositBalance <- holdUniqDyn $
        zipDynWith (-) (getDeposit dCoinsBurn) (getDeposit dCoinsMint)
      holdUniqDyn $ zipDynWith (+) dEncoinsBalance dDepositBalance

getBulletproofParams :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t (Maybe Address)
  -> Dynamic t Integer
  -> m (Dynamic t BulletproofParams)
getBulletproofParams dWallet dmAddress dFees = do
    let dAddrWallet = zipDynWith fromMaybe (fmap walletChangeAddress dWallet) dmAddress
        f aL aC fees = addressToBytes aL `Text.append` addressToBytes aC `Text.append` encodeHex (fromBuiltin $ toBytes fees)
        dPar = zipDynWith (f ledgerAddress) dAddrWallet dFees
    performEvent_ (flip sha2_256 "bulletproofParamsElement" <$> updated dPar)
    elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)

getRandomness :: MonadWidget t m => Event t () -> m (Behavior t Randomness)
getRandomness e = do
    eRandomness <- performEvent $ liftIO randomIO <$ e
    hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

getEnvironment :: MonadWidget t m
  => EncoinsMode
  -> Dynamic t Wallet
  -> Dynamic t (Maybe Address)
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> m ( Dynamic t Integer
       , Dynamic t Integer
       , Dynamic t BulletproofParams
       , Behavior t Randomness
       )
getEnvironment mode dWallet dmChangeAddr dCoinsBurn dCoinsMint = do
  dBalance <- getBalance mode dCoinsBurn dCoinsMint
  let dFees = fmap (protocolFees mode) dBalance
  dTotalBalance <- holdUniqDyn $ zipDynWith (-) dBalance dFees
  dBulletproofParams <- getBulletproofParams dWallet dmChangeAddr dFees
  bRandomness        <- getRandomness $ void $ updated dBulletproofParams
  return (dTotalBalance, dFees, dBulletproofParams, bRandomness)
