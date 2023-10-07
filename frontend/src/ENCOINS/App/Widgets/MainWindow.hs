module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                          ((<=<))
import           Data.Functor                           ((<&>))
import           Data.Text                              (Text)
import           Reflex.Dom
import           Data.ByteString.Lazy                   (toStrict)
import           Data.Text.Encoding                     (decodeUtf8)
import           Data.Aeson                             (encode)
import           Data.List                              (nub)

import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (loadAppData, elementResultJS, tellTxStatus )
import           ENCOINS.App.Widgets.MainTabs           (walletTab, transferTab, ledgerTab)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.TabsSelection      (AppTab(..), tabsSection)
import           ENCOINS.Bulletproofs                   (Secret, Secrets)
import           Backend.Status                         (Status (CustomStatus))
import ENCOINS.Common.Events
import           JS.Website                             (saveJSON)
import           ENCOINS.App.Widgets.Coin               (coinWithName)
import Reflex.Dom (MonadHold(holdDyn), Reflex (Behavior))
import           Backend.Status                         (Status(..))


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

      let bIsEmptySecretsWithName = current $ null <$> dOldSecretsWithName
      logDyn "null <$> dOldSecretsWithName" $ null <$> dOldSecretsWithName

      eConvertedSecrets <- loadCache mPass bIsEmptySecretsWithName
      logEvent "eConvertedSecrets" $ take 1 <$> eConvertedSecrets
      dConvertedSecrets <- holdDyn [] $ gate bIsEmptySecretsWithName eConvertedSecrets
      -- logDyn "dConvertedSecrets" $ take 1 <$> dConvertedSecrets

      let dOldSecretsWithNameAll = nub <$> mconcat [dOldSecretsWithName, dConvertedSecrets]
      -- let dOldSecretsWithNameAll = mconcat [dOldSecretsWithName]
      logDyn "dOldSecretsWithNameAll" $ take 1 <$> dOldSecretsWithNameAll

      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecretsWithNameAll
        TransferTab -> transferTab mPass dWallet dOldSecretsWithNameAll
        LedgerTab   -> ledgerTab mPass dWallet dOldSecretsWithNameAll
      return $ updated dOldSecretsWithNameAll
    holdDyn [] eSecretsWithName

loadCache :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Behavior t Bool
  -> m (Event t [(Secret, Text)])
loadCache mPass bIsEmptySecretsWithName = do
  dOldSecrets <- loadAppData (getPassRaw <$> mPass) "encoins" id []
  let dConvertedSecrets = map coinWithName <$> dOldSecrets

  let eConvertedSecrets = gate bIsEmptySecretsWithName $ updated dConvertedSecrets
  -- logEvent "eConvertedSecrets" $ take 1 <$> eConvertedSecrets

  -- let statusEvent' = CustomStatus "Cache structure is updating. Please wait." <$ eConvertedSecrets
  -- let statusEvent = gate bIsEmptySecretsWithName statusEvent'
  -- logEvent "statusEvent" statusEvent
  -- tellTxStatus "Wallet status" Ready statusEvent

  performEvent_ $
      saveJSON (getPassRaw <$> mPass) "encoins-with-name"
        . decodeUtf8
        . toStrict
        . encode <$> eConvertedSecrets
  pure eConvertedSecrets