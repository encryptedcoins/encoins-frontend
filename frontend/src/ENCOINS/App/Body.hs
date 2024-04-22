{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson                         (ToJSON)
import           Data.Bifunctor                     (first)
import           Data.Maybe                         (isNothing)
import           Data.Text                          (Text)
import           Reflex.Dom

import           Backend.Protocol.StrongTypes       (toPasswordHash)
import           Backend.Protocol.Types
import           Backend.Utility                    (switchHoldDyn)
import           Backend.Wallet                     (walletsSupportedInApp)
import           ENCOINS.App.Widgets.Basic          (loadAppDataE,
                                                     waitForScripts)
import           ENCOINS.App.Widgets.CloudWindow    (cloudSettingsWindow)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.Notification
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWallet,
                                                     welcomeWindow,
                                                     welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Cache               (aesKey, encoinsV3,
                                                     isCloudOn,
                                                     passwordSotrageKey)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toJsonText)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification)
import           ENCOINS.Common.Widgets.Basic       (notification)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           ENCOINS.Common.Widgets.MoreMenu    (WindowMoreMenuClass (..),
                                                     moreMenuWindow)
import           JS.App                             (loadCacheValue)
import           JS.Website                         (saveJSON)

bodyContentWidget :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mPass = mdo
  (ePassOpen, eConnectOpen, eCloudOpen, eMoreMenuOpen) <- navbarWidget
    dWallet
    dIsDisableButtons
    mPass
    dCloudOn
    dCloudStatus

  let moreMenuClass = WindowMoreMenuClass
        "common-MoreMenu_Window"
        "common-MoreMenu_LinkContainer"
        "common-MoreMenu_Link"
  moreMenuWindow moreMenuClass eMoreMenuOpen

  (dStatusT, dIsDisableButtons, dCloudStatus) <-
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
    dCloudOn
    dmKey
    dResetTokens
    eRestore

  let eReEncrypt = leftmost [eNewPass, Nothing <$ eCleanOk]

  performEvent_
    $ fmap (reEncrypt encoinsV3)
    $ attachPromptlyDyn dTokensV3 eReEncrypt

  performEvent_
    $ fmap (reEncrypt aesKey)
    $ attachPromptlyDynWithMaybe (\mKey e -> (,e) <$> mKey) dmKey eReEncrypt

  copiedNotification

  dSaveOnFromCache <- loadAppDataE Nothing isCloudOn "app-body-load-is-save-on-key" id False
  dmOldKeyBody <- loadAppDataE mPass aesKey "app-body-load-of-aes-key" id Nothing

  (dSaveWindow, dNewKeyWindow, eRestore) <- cloudSettingsWindow
    mPass
    dSaveOnFromCache
    dCloudStatus
    eCloudOpen
  dCloudOn <- holdDyn False $ leftmost $ map updated [dSaveOnFromCache, dSaveWindow]

  dResetTokens <- holdDyn False $ updated $ isNothing <$> dmOldKeyBody

  dmKey <- holdUniqDyn =<< (holdDyn Nothing $ leftmost $ map updated [dmOldKeyBody, dNewKeyWindow])

  pure $ leftmost [Nothing <$ eCleanOk, eNewPass]

-- ReEncryption functions wanted every time when
-- wherever some key is saved with password.
reEncrypt :: (ToJSON a, MonadIO m) => Text -> (a, Maybe PasswordRaw) -> m ()
reEncrypt key (d, mNewPass) = saveJSON
      (getPassRaw <$> mNewPass) key . toJsonText $ d

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  mPass <- toPasswordHash <$> loadCacheValue passwordSotrageKey
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
