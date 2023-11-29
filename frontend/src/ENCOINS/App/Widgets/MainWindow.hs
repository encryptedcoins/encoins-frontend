{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Aeson                         (encode)
import           Data.Bool                          (bool)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Reflex.Dom

import           Backend.Status                     (Status (..))
import           Backend.Utility                    (switchHoldDyn)
import           Backend.Wallet                     (Wallet (..))
import           ENCOINS.App.Widgets.Basic          (elementResultJS,
                                                     loadAppData, tellTxStatus)
import           ENCOINS.App.Widgets.Coin           (coinWithName)
import           ENCOINS.App.Widgets.MainTabs       (ledgerTab, transferTab,
                                                     walletTab)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw (..))
import           ENCOINS.App.Widgets.TabsSelection  (AppTab (..), tabsSection)
import           ENCOINS.Bulletproofs               (Secret)
import           JS.Website                         (saveJSON)


mainWindow :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t Bool
  -> Dynamic t [Text]
  -> m (Dynamic t [(Secret, Text)])
mainWindow mPass dWallet dIsDisableButtons dUrls = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eSecretsWithName <- switchHoldDyn dTab $ \tab -> mdo
      dOldSecretsWithName <- loadAppData (getPassRaw <$> mPass) "encoins-with-name" id []

      updateCache mPass dOldSecretsWithName

      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecretsWithName dUrls
        TransferTab -> transferTab mPass dWallet dOldSecretsWithName dUrls
        LedgerTab   -> ledgerTab mPass dOldSecretsWithName dUrls
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
  tellTxStatus "App status" statusEvent


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
  tellTxStatus "App status" $
    CustomStatus "Please reload the page" <$ leftmost [eSecretsIsEmptyLog, eSaved]
