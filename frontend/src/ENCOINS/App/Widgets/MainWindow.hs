module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Control.Monad.Extra                    (whenM)
import           Data.Functor                           ((<&>))
import           Data.Text                              (Text)
import           Data.Maybe                             (isNothing)
import           Reflex.Dom

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (loadAppData)
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secrets)
import           Backend.Servant.Client                 (getRelayUrl)
import           ENCOINS.Common.Events                  (newEvent)


mainWindow :: (MonadWidget t m , EventWriter t Text m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> m (Dynamic t Secrets)
mainWindow mPass dWallet = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab

    whenM (isNothing <$> getRelayUrl) $ do
      ev <- newEvent
      tellEvent $
        "All available relays are down! Try reloading the page or come back later." <$ ev

    eSecrets <- switchHold never <=< dyn $ dTab <&> \tab -> do
      dOldSecrets <- loadAppData (getPassRaw <$> mPass) "encoins" id []
      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecrets
        TransferTab -> transferTab mPass dWallet dOldSecrets
        LedgerTab   -> ledgerTab mPass dWallet dOldSecrets
      return $ updated dOldSecrets
    holdDyn [] eSecrets