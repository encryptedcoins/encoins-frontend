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
import           ENCOINS.App.Widgets.IPFS
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.Notification
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWallet,
                                                     welcomeWindow,
                                                     welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Cache               (encoinsV3, ipfsCacheKey,
                                                     isIpfsOn)
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
  (ePassOpen, eConnectOpen, eOpenIpfsWindow) <- navbarWidget
    dWallet
    dIsDisableButtons
    mPass
    dIpfsOn
    dIpfsSaveStatus

  (dStatusT, dIsDisableButtons, dIpfsSaveStatus) <-
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
    dIpfsOn
    dmKey

  let eReEncrypt = leftmost [eNewPass, Nothing <$ eCleanOk]

  performEvent_
    $ fmap (reEncrypt encoinsV3)
    $ attachPromptlyDyn dTokensV3 eReEncrypt

  performEvent_
    $ fmap (reEncrypt ipfsCacheKey)
    $ attachPromptlyDynWithMaybe (\mKey e -> (,e) <$> mKey) dmKey eReEncrypt

  copiedNotification

  dIpfsCache <- loadAppDataE Nothing isIpfsOn "app-body-load-is-ipfs-on-key" id False
  (dIpfsWindow, dAesKeyWindow) <- ipfsSettingsWindow
    mPass
    dIpfsCache
    dIpfsSaveStatus
    eOpenIpfsWindow
  dIpfsOn <- holdDyn False $ leftmost $ map updated [dIpfsCache, dIpfsWindow]

  dmKeyCache <- loadAppDataE mPass ipfsCacheKey "app-body-load-of-aes-key" id Nothing
  logDyn "body: dmKeyCache" dmKeyCache
  logDyn "body: dAesKeyWindow" dAesKeyWindow
  dmKey <- holdUniqDyn =<< (holdDyn Nothing $ leftmost $ map updated [dmKeyCache, dAesKeyWindow])
  logDyn "body: dmKey" dmKey

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
