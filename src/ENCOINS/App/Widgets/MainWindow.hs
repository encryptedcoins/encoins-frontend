module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Control.Monad.IO.Class                 (MonadIO(..))
import           Data.Functor                           ((<&>))
import           Reflex.Dom
import           System.Random.Stateful                 (randomIO)

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (loadAppData)
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secrets, Randomness(..))
import           ENCOINS.Crypto.Field                   (Field(..))

mainWindow :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  m (Dynamic t Secrets)
mainWindow mpass dWallet = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab
    -- Obtaining Randomness
    let dBulletproofParams = walletBulletproofParams <$> dWallet
    eRandomness <- performEvent $ liftIO randomIO <$ updated dBulletproofParams
    bRandomness <- hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    eSecrets <- switchHold never <=< dyn $ dTab <&> \tab -> do
      dOldSecrets <- loadAppData (getPassRaw <$> mpass) "encoins" id []
      case tab of
        WalletTab   -> walletTab mpass dWallet dOldSecrets bRandomness
        TransferTab -> transferTab mpass dWallet dOldSecrets
        LedgerTab   -> ledgerTab mpass dWallet dOldSecrets bRandomness
      return (updated dOldSecrets)
    holdDyn [] eSecrets
