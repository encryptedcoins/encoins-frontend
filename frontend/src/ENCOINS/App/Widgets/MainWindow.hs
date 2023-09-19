module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Data.Functor                           ((<&>))
import           Reflex.Dom

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (loadAppData)
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secrets)

mainWindow :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  m (Dynamic t Secrets)
mainWindow mPass dWallet = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab

    eSecrets <- switchHold never <=< dyn $ dTab <&> \tab -> do
      dOldSecrets <- loadAppData (getPassRaw <$> mPass) "encoins" id []
      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecrets
        TransferTab -> transferTab mPass dWallet dOldSecrets
        LedgerTab   -> ledgerTab mPass dWallet dOldSecrets
      return (updated dOldSecrets)
    holdDyn [] eSecrets