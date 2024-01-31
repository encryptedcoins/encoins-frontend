{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body (bodyWidget) where

import           Data.Aeson                         (encode)
import           Data.Bifunctor                     (first)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8)
import           Reflex.Dom

import           Backend.Status                     (Status (..), isNoRelay,
                                                     isReady,
                                                     isTxProcessOrCriticalError)
import           Backend.Utility                    (switchHoldDyn)
import           Backend.Wallet                     (Wallet (..),
                                                     walletsSupportedInApp)
import           Config.Config                      (NetworkConfig (..),
                                                     networkConfig)
import           ENCOINS.App.Widgets.Basic          (elementResultJS,
                                                     waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.IPFS
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWallet,
                                                     welcomeWindow,
                                                     welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Cache               (encoinsV3)
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification)
import           ENCOINS.Common.Widgets.Basic       (column, notification,
                                                     space)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           JS.App                             (loadHashedPassword)
import           JS.Website                         (saveJSON)

import           ENCOINS.Common.Events

bodyContentWidget :: MonadWidget t m => Maybe PasswordRaw -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mPass = mdo
  (ePassOpen, eConnectOpen, eIpfsOpen) <- navbarWidget dWallet dIsDisableButtons mPass

  (dStatusT, dIsDisableButtons) <- handleAppStatus dWallet evEvStatus
  notification dStatusT

  dWallet <- connectWindow walletsSupportedInApp eConnectOpen

  (eNewPass, eClearCache) <- passwordSettingsWindow ePassOpen
  eCleanOk <- cleanCacheDialog eClearCache
  -- logEvent "body: eNewPass" eNewPass
  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  (dSecrets, evEvStatus) <- runEventWriterT $ mainWindow mPass dWallet dIsDisableButtons dmKey

  performEvent_ (reEncryptEncoins <$> attachPromptlyDyn dSecrets (leftmost
    [eNewPass, Nothing <$ eCleanOk]))

  copiedNotification

  -- logEvent "body: eIpfsOpen" eIpfsOpen

  dIpfsOn <- ipfsSettingsWindow mPass eIpfsOpen

  ev <- newEventWithDelay 1.5
  dmKey <- fetchAesKey mPass $ leftmost [ev, () <$ updated dIpfsOn]
  -- dmKey <- holdDyn Nothing emKey
  -- logDyn "body: dmKey" dmKey
  -- dRestoredTokens <- restoreValidTokens dKey (walletAddressBech32 <$> dWallet)
  -- logDyn "body: dRestoredTokens" dRestoredTokens

  pure $ leftmost [Nothing <$ eCleanOk, eNewPass]

  where
    reEncryptEncoins (d, mNewPass) = saveJSON (getPassRaw <$> mNewPass) encoinsV3
      . decodeUtf8 .  toStrict . encode $ d

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

fetchWalletNetworkStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t (Text, Status))
fetchWalletNetworkStatus dWallet = do
  dWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
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
    -- Hold NoRelay status once it fired until page reloading.
    (\ev (_, accS) -> if isNoRelay accS then Nothing else Just ev)
    (T.empty, Ready) $ leftmost [eStatus, updated dWalletNetworkStatus]

  let flatStatus (t, s)
        | isReady s = T.empty
        | T.null $ T.strip t = toText s
        | otherwise = t <> column <> space <> toText s

  let dIsDisableButtons = (isTxProcessOrCriticalError . snd) <$> dStatus
  let dStatusT = flatStatus <$> dStatus

  pure (dStatusT, dIsDisableButtons)
