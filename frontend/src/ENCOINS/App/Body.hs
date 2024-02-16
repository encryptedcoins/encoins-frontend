{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Bifunctor                     (first)
import qualified Data.List.NonEmpty                 as NE
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Reflex.Dom

import           Backend.Protocol.Types
import           Backend.Status                     (AppStatus,
                                                     IpfsSaveStatus (..),
                                                     Status (..),
                                                     isIpfsSaveStatus,
                                                     isNoRelay, isReady,
                                                     isStatus,
                                                     isTxProcessOrCriticalError)
import           Backend.Utility                    (column, space,
                                                     switchHoldDyn, toText)
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

  logDyn "body: dIpfsSaveStatus" dIpfsSaveStatus

  dWallet <- connectWindow walletsSupportedInApp eConnectOpen

  (eNewPass, eClearCache) <- passwordSettingsWindow ePassOpen
  eCleanOk <- cleanCacheDialog eClearCache
  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  (dSecretsV3, evStatusList) <- runEventWriterT $ mainWindow
    mPass
    dWallet
    dIsDisableButtons
    dIpfsOn
    dmKey

  let eReEncrypt = leftmost [eNewPass, Nothing <$ eCleanOk]

  performEvent_
    $ fmap reEncryptEncoins
    $ attachPromptlyDyn dSecretsV3 eReEncrypt

  copiedNotification

  evIpfs <- postDelay 0.2
  dIpfsCache <- fetchIpfsFlag "app-body-load-is-ipfs-on-key" evIpfs
  (dIpfsWindow, dAesKeyWindow) <- ipfsSettingsWindow
    mPass
    dIpfsCache
    dIpfsSaveStatus
    eOpenIpfsWindow
  dIpfsOn <- holdDyn False $ leftmost $ map updated [dIpfsCache, dIpfsWindow]

  evKey <- postDelay 0.2
  dmKeyCache <- fetchAesKey mPass "app-body-load-of-aes-key" evKey
  dmKey <- holdDyn Nothing $ leftmost $ map updated [dmKeyCache, dAesKeyWindow]

  pure $ leftmost [Nothing <$ eCleanOk, eNewPass]

  where
    reEncryptEncoins :: MonadIO m
      => ([TokenCacheV3], Maybe PasswordRaw)
      -> m ()
    reEncryptEncoins (d, mNewPass) = saveJSON
      (getPassRaw <$> mNewPass) encoinsV3 . toJsonText $ d

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
  -> Event t [AppStatus]
  -> m (Dynamic t Text, Dynamic t Bool, Dynamic t IpfsSaveStatus)
handleAppStatus dWallet elAppStatus = do
  -- logEvent "handleAppStatus: elAppStatus" elAppStatus
  let (eStatus, eIpfsStatus) = partitionAppStatus elAppStatus
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

  dIpfsStatus <- holdDyn TurnOff eIpfsStatus

  pure (dStatusT, dIsDisableButtons, dIpfsStatus)

partitionAppStatus :: Reflex t
  => Event t [AppStatus]
  -> (Event t (Text, Status), Event t IpfsSaveStatus)
partitionAppStatus ev =
    ( filterMost ("", Ready) isStatus <$> ev
    , filterMost TurnOff isIpfsSaveStatus <$> ev
    )
  where
    filterMost def f = maybe def NE.head . NE.nonEmpty . mapMaybe f
