module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Data.Functor                           ((<&>))
import           Data.Text                              (Text)
import           Reflex.Dom
import           Data.Bool                              (bool)
import           Data.ByteString.Lazy                   (toStrict)
import           Data.Text.Encoding                     (decodeUtf8)
import           Data.Aeson                             (encode)

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (loadAppData, elementResultJS, tellTxStatus )
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secret)
import           Backend.Status                         (Status (..))
import           JS.Website                             (saveJSON)
import           ENCOINS.App.Widgets.Coin               (coinWithName)


mainWindow :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t Bool
  -> m (Dynamic t [(Secret, Text)])
mainWindow mPass dWallet dIsDisableButtons = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eSecretsWithName <- switchHold never <=< dyn $ dTab <&> \tab -> mdo
      dOldSecretsWithName <- loadAppData (getPassRaw <$> mPass) "encoins-with-name" id []

      updateCache mPass dOldSecretsWithName

      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecretsWithName
        TransferTab -> transferTab mPass dWallet dOldSecretsWithName
        LedgerTab   -> ledgerTab mPass dWallet dOldSecretsWithName
      return $ updated dOldSecretsWithName
    holdDyn [] eSecretsWithName

-- Update cache only when
-- "encoins-with-name" cache doesn't exist
--  and
-- "encoins" cache exists.
updateCache :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t [(Secret, Text)]
  -> m ()
updateCache mPass dSecretsWithName = do
  let eSecretsWithNameIsEmpty = updated $ null <$> dSecretsWithName
  widgetHold_ blank $
    bool blank (loadCache mPass) <$> eSecretsWithNameIsEmpty

loadCache :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> m ()
loadCache mPass = do
  eSecrets <- updated <$> loadAppData (getPassRaw <$> mPass) "encoins" id []

  let eSecretsIsEmpty = null <$> eSecrets

  let statusEvent = bool
        (CustomStatus "Cache structure is updating. Please wait.")
        Ready
        <$> eSecretsIsEmpty
  tellTxStatus "App status" Ready statusEvent


  -- Convert "encoins" cache when it is not empty
  let eConvertedSecrets = coincidence $
        bool (map coinWithName <$> eSecrets) never <$> eSecretsIsEmpty

  performEvent_ $
      saveJSON (getPassRaw <$> mPass) "encoins-with-name"
        . decodeUtf8
        . toStrict
        . encode <$> eConvertedSecrets

  -- Ask user to reload when cache structure is updated
  let eSecretsIsEmptyLog = () <$ ffilter id eSecretsIsEmpty
  eSaved <- updated <$> elementResultJS "encoins-with-name" (const ())
  tellTxStatus "App status" Ready $
    CustomStatus "Please reload the page" <$ leftmost [eSecretsIsEmptyLog, eSaved]