module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad                      ((<=<))
import           Data.Aeson                         (encode)
import           Data.Bifunctor                     (first)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Functor                       ((<&>))
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Bool                          (bool)
import           Reflex.Dom
import qualified Data.Text as T

import           Backend.Status                     (Status(..), isStatusBusyWithBackendError, isReady, isBackendError)
import           Backend.Wallet                     (walletsSupportedInApp)
import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWindow, welcomeWallet, welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Widgets.Basic       (notification, space, column)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           JS.App                             (loadHashedPassword)
import           JS.Website                         (saveJSON)

bodyContentWidget :: MonadWidget t m => Maybe PasswordRaw -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mpass = mdo
  (eSettingsOpen, eConnectOpen) <- navbarWidget dWallet mpass

  let eStatus = coincidence $ leftmost <$> evEvStatus
  dStatus <- foldDynMaybe
    -- Hold BackendError status once it fired until page reloading.
    (\ev (_, accS) -> if isBackendError accS then Nothing else Just ev)
    (T.empty, Ready) eStatus
  notification $ flatStatus <$> dStatus

  let dIsDisableButtons = (isStatusBusyWithBackendError . snd) <$> dStatus

  dWallet <- connectWindow walletsSupportedInApp eConnectOpen
  (eNewPass, eResetPass) <- passwordSettingsWindow eSettingsOpen
  eCleanOk <- cleanCacheDialog eResetPass

  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  (dSecrets, evEvStatus) <- runEventWriterT $ mainWindow mpass dWallet dIsDisableButtons

  performEvent_ (reEncryptEncoins <$> attachPromptlyDyn dSecrets (leftmost
    [eNewPass, Nothing <$ eCleanOk]))

  copiedNotification

  pure $ leftmost [Nothing <$ eCleanOk, eNewPass]

  where
    reEncryptEncoins (d, mNewPass) = saveJSON (getPassRaw <$> mNewPass) "encoins"
      . decodeUtf8 .  toStrict . encode $ d
    flatStatus (t, s) = bool (t <> column <> space <> T.pack (show s)) T.empty $ isReady s

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  mPass <- fmap PasswordHash <$> loadHashedPassword passwordSotrageKey
  (ePassOk, eReset) <- case mPass of
    Just pass -> first (Just <$>) <$> enterPasswordWindow pass eCleanOk
    Nothing -> do
      ePb <- getPostBuild
      pure (Nothing <$ ePb, never)
  eCleanOk <- cleanCacheDialog eReset
  dmmPass <- holdDyn Nothing $ Just <$> leftmost [ePassOk, Nothing <$ eCleanOk, eNewPass]
  eNewPass <- switchHold never <=< dyn $ dmmPass <&> \case
    Nothing -> pure never
    Just mpass -> bodyContentWidget mpass

  jQueryWidget
