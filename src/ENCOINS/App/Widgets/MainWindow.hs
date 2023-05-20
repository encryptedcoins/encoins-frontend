module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Data.Functor                           ((<&>))
import           Reflex.Dom

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secrets)

mainWindow :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m ()
mainWindow mpass dWallet dOldSecrets = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab

    eSecretsWithNamesInTheWallet <- switchHold never <=< dyn $ dTab <&> \case
      WalletTab   -> walletTab mpass dWallet dOldSecrets
      TransferTab -> transferTab mpass dWallet dSecretsWithNamesInTheWallet dOldSecrets
      LedgerTab   -> ledgerTab mpass dWallet dOldSecrets
    dSecretsWithNamesInTheWallet <- holdDyn [] eSecretsWithNamesInTheWallet
    blank
