{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson                         (ToJSON)
import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import           Reflex.Dom

import           Backend.Protocol.Types
import           Backend.Utility                    (switchHoldDyn)
import           Backend.Wallet                     (walletsSupportedInApp)
import           ENCOINS.App.Widgets.Basic          (loadAppDataE,
                                                     waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.Save
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.Notification
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWallet,
                                                     welcomeWindow,
                                                     welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Cache               (encoinsV3, saveKey,
                                                     isSaveOn)
import           ENCOINS.Common.Utils               (toJsonText)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification)
import           ENCOINS.Common.Widgets.Basic       (notification)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           JS.App                             (loadHashedPassword)
import           JS.Website                         (saveJSON)

import           ENCOINS.Common.Events

bodyContentWidget :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mPass = mdo
  (ePassOpen, eConnectOpen, eOpenSaveWindow) <- navbarWidget
    dWallet
    dIsDisableButtons
    mPass
    dSaveOn
    dSaveStatus

  (dStatusT, dIsDisableButtons, dSaveStatus) <-
    handleAppStatus dWallet evStatusList
  notification dStatusT

  dWallet <- connectWindow walletsSupportedInApp eConnectOpen

  (eNewPass, eClearCache) <- passwordSettingsWindow ePassOpen
  eCleanOk <- cleanCacheDialog eClearCache
  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  (dTokensV3, evStatusList) <- runEventWriterT $ mainWindow
    mPass
    dWallet
    dIsDisableButtons
    dSaveOn
    dmKey
    dKeyTheSame

  let eReEncrypt = leftmost [eNewPass, Nothing <$ eCleanOk]

  performEvent_
    $ fmap (reEncrypt encoinsV3)
    $ attachPromptlyDyn dTokensV3 eReEncrypt

  performEvent_
    $ fmap (reEncrypt saveKey)
    $ attachPromptlyDynWithMaybe (\mKey e -> (,e) <$> mKey) dmKey eReEncrypt

  copiedNotification

  dSaveOnFromCache <- loadAppDataE Nothing isSaveOn "app-body-load-is-save-on-key" id False
  (dSaveWindow, dAesKeyWindow, dKeyTheSame) <- saveSettingsWindow
    mPass
    dSaveOnFromCache
    dSaveStatus
    eOpenSaveWindow
  dSaveOn <- holdDyn False $ leftmost $ map updated [dSaveOnFromCache, dSaveWindow]

  dmKeyCache <- loadAppDataE mPass saveKey "app-body-load-of-aes-key" id Nothing
  dmKey <- holdUniqDyn =<< (holdDyn Nothing $ leftmost $ map updated [dmKeyCache, dAesKeyWindow])

  pure $ leftmost [Nothing <$ eCleanOk, eNewPass]

-- ReEncryption functions wanted every time when
-- wherever some key is saved with password.
reEncrypt :: (ToJSON a, MonadIO m) => Text -> (a, Maybe PasswordRaw) -> m ()
reEncrypt key (d, mNewPass) = saveJSON
      (getPassRaw <$> mNewPass) key . toJsonText $ d

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  mPass <- fmap PasswordHash <$> loadHashedPassword passwordSotrageKey
  (ePassOk, eCleanCache) <- case mPass of
    Just pass -> first (Just <$>) <$> enterPasswordWindow pass eCleanOk
    Nothing -> do
      ePb <- getPostBuild
      pure (Nothing <$ ePb, never)
  eCleanOk <- cleanCacheDialog eCleanCache
  dmmPass <- holdDyn Nothing $ Just <$> leftmost [ePassOk, Nothing <$ eCleanOk, eNewPass]
  eNewPass <- switchHoldDyn dmmPass $ \case
    Nothing   -> pure never
    Just pass -> bodyContentWidget pass
  jQueryWidget
