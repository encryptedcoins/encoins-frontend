{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Aeson                         (encode)
import           Data.Bool                          (bool)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Reflex.Dom

import           Backend.Protocol.Types             (TokenCache)
import           Backend.Status                     (Status (..))
import           Backend.Utility                    (switchHoldDyn)
import           Backend.Wallet                     (Wallet (..))
import           ENCOINS.App.Widgets.Basic          (elementResultJS,
                                                     loadAppData, tellTxStatus)
import           ENCOINS.App.Widgets.Coin           (coinWithName, coinV3)
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
  -> m (Dynamic t [(Secret, Text)])
mainWindow mPass dWallet dIsDisableButtons = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eSecretsWithName <- switchHoldDyn dTab $ \tab -> mdo
      -- dSecretsV3 <- loadAppData (getPassRaw <$> mPass) "encoins-v3" id []

      dOldSecretsWithName <- loadAppData (getPassRaw <$> mPass) "encoins-with-name" id []

      updateCache mPass dOldSecretsWithName

      case tab of
        WalletTab   -> walletTab mPass dWallet dOldSecretsWithName
        TransferTab -> transferTab mPass dWallet dOldSecretsWithName
        LedgerTab   -> ledgerTab mPass dOldSecretsWithName
      return $ updated dOldSecretsWithName
    holdDyn [] eSecretsWithName

{-
Evolutions of encoins cache by key
1. encoins - first version of cache that contains Secrets only
2. encoins-with-cache - second version of cache that contains (Secret, AssetName)
3. encoins-v3 - third version of the cache
   that contains record with AssetName, Secret, IpfsStatus (pinned/unpinned) and CoinStatus (minted/burned)
-}

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

-- Update cache only when
-- "encoins-v3" cache doesn't exist
-- and
-- "encoins-with-name" cache exists
--  or
-- "encoins" cache exists.
updateCacheV3 :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t [TokenCache]
  -> m ()
updateCacheV3 mPass dSecretsV3 = do
  let eSecretsV3 = updated $ null <$> dSecretsV3
  widgetHold_ blank $
    bool blank (loadCacheV3 mPass) <$> eSecretsV3

loadCacheV3 :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> m ()
loadCacheV3 mPass = do
  eSecretsV1 <- updated <$> loadAppData (getPassRaw <$> mPass) "encoins" id []

  let eEncoinsIsEmpty = null <$> eSecretsV1

  let eStatusV1 = bool
        (CustomStatus "Cache structure is updating. Please wait.")
        Ready
        <$> eEncoinsIsEmpty
  tellTxStatus "App status" eStatusV1

  -- Convert "encoins" cache (when it is not empty) to encoins-v3
  let eSecretsV3 = coincidence $
        bool (map coinV3 <$> eSecretsV1) never <$> eEncoinsIsEmpty

  performEvent_ $
      saveJSON (getPassRaw <$> mPass) "encoins-v3"
        . decodeUtf8
        . toStrict
        . encode <$> eSecretsV3

  -- Ask user to reload when cache structure is updated
  let eEncoinsIsEmptyLog = () <$ ffilter id eEncoinsIsEmpty
  eSaved <- updated <$> elementResultJS "encoins-v3" (const ())
  tellTxStatus "App status" $
    CustomStatus "Please reload the page" <$ leftmost [eEncoinsIsEmptyLog, eSaved]
