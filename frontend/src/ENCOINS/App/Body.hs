module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad                      ((<=<))
import           Data.Aeson                         (encode)
import           Data.Bifunctor                     (first)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Functor                       ((<&>))
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Bool                          (bool)
import           Data.Text                          (Text)
import           Reflex.Dom
import qualified Data.Text as T

import           Backend.Status                     (Status(..), isStatusBusyBackendNetwork, isReady, isBlockError)
import           Backend.Wallet                     (walletsSupportedInApp, Wallet(..), networkConfig, NetworkConfig(..))
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.App.Widgets.Basic          (waitForScripts, elementResultJS)
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
import ENCOINS.Common.Events


bodyContentWidget :: MonadWidget t m => Maybe PasswordRaw -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mpass = mdo
  (eSettingsOpen, eConnectOpen) <- navbarWidget dWallet mpass

  (dStatusT, dIsDisableButtons) <- handleAppStatus dWallet evEvStatus

  notification dStatusT

  dWallet <- connectWindow walletsSupportedInApp eConnectOpen
  logDyn "dWallet" $ walletName <$> dWallet

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
    reEncryptEncoins (d, mNewPass) = saveJSON (getPassRaw <$> mNewPass) "encoins-with-name"
      . decodeUtf8 .  toStrict . encode $ d

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

fetchWalletNetworkStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t (Text, Status))
fetchWalletNetworkStatus dWallet = do
  eWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated eWalletLoad
  let eUnexpectedNetworkB = fmap
        (\w -> walletNetworkId w /= app networkConfig)
        eLoadedWallet
  let mkNetworkMessage isInvalidNetwork message =
        case (isInvalidNetwork, message) of
          (True,_) -> Just ("NetworkId status", WalletNetworkError unexpectedNetworkApp)
          (False, ("", Ready)) -> Nothing
          (False, _) -> Just (T.empty, Ready)
  foldDynMaybe mkNetworkMessage (T.empty, Ready) eUnexpectedNetworkB

unexpectedNetworkApp :: Text
unexpectedNetworkApp =
           "Unexpected network! Please switch the wallet to"
        <> space
        <> toText (app networkConfig)
        <> space
        <> "mode."

handleAppStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> Event t [Event t (Text, Status)]
  -> m (Dynamic t Text, Dynamic t Bool)
handleAppStatus dWallet evEvStatus = do
  let eStatus = coincidence $ leftmost <$> evEvStatus
  dWalletNetworkStatus <- fetchWalletNetworkStatus dWallet

  dStatus <- foldDynMaybe
    -- Hold BackendError status once it fired until page reloading.
    (\ev (_, accS) -> if isBlockError accS then Nothing else Just ev)
    (T.empty, Ready) $ leftmost [eStatus, updated dWalletNetworkStatus]

  let flatStatus (t, s) = bool (t <> column <> space <> T.pack (show s)) T.empty $ isReady s

  let dIsDisableButtons = (isStatusBusyBackendNetwork . snd) <$> dStatus
  let dStatusT = flatStatus <$> dStatus

  pure (dStatusT, dIsDisableButtons)