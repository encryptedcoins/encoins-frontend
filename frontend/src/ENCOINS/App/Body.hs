{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body
    ( bodyWidget
    ) where

import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import Reflex.Dom

import Backend.Protocol.StrongTypes (toPasswordHash)
import Backend.Protocol.Types (PasswordRaw (..))
import Backend.Utility (switchHoldDyn)
import Backend.Status (AppStatus(..))
import Backend.Wallet (walletsSupportedInApp)
import ENCOINS.App.Widgets.Basic
    ( loadAppDataE
    , waitForScripts
    )
import ENCOINS.App.Widgets.CloudWindow (cloudSettingsWindow)
import ENCOINS.App.Widgets.ConnectWindow (connectWindow)
import ENCOINS.App.Widgets.MainWindow (mainWindow)
import ENCOINS.App.Widgets.Navbar (navbarWidget)
import ENCOINS.App.Widgets.Notification
import ENCOINS.App.Widgets.PasswordWindow
import ENCOINS.App.Widgets.ReEncryption
    ( reEncryptCurrentCache
    , reEncryptOldCache
    )
import ENCOINS.App.Widgets.WelcomeWindow
    ( welcomeWallet
    , welcomeWindow
    , welcomeWindowWalletStorageKey
    )
import ENCOINS.Common.Cache
    ( aesKey
    , isCloudOn
    , passwordStorageKey
    )
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Advanced (copiedNotification)
import ENCOINS.Common.Widgets.Basic (notification)
import ENCOINS.Common.Widgets.JQuery (jQueryWidget)
import ENCOINS.Common.Widgets.MoreMenu
    ( WindowMoreMenuClass (..)
    , moreMenuWindow
    )
import JS.App (loadCacheValue)

bodyContentWidget ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mPass = mdo
    (ePassOpen, eConnectOpen, eCloudOpen, eMoreMenuOpen) <-
        navbarWidget
            dWallet
            dIsDisableButtons
            mPass
            dCloudOn
            dCloudStatus

    let moreMenuClass =
            WindowMoreMenuClass
                "common-MoreMenu_Window"
                "common-MoreMenu_LinkContainer"
                "common-MoreMenu_Link"
    moreMenuWindow moreMenuClass eMoreMenuOpen

    (dStatusT, dIsDisableButtons, dCloudStatus) <-
        handleAppStatus dWallet evStatusList $
            leftmost
                [ CustomStatus "Re-encrypting cache with new password..." <$ eReEncrypt
                , AppReady <$ eReEncryptDelayed
                ]
    notification dStatusT

    dWallet <- connectWindow walletsSupportedInApp eConnectOpen

    (eNewPass, eClearCache) <- passwordSettingsWindow ePassOpen
    eCleanOk <- cleanCacheDialog eClearCache
    welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

    divClass "section-app section-app-empty wf-section" blank

    (dTokensV3, evStatusList) <-
        runEventWriterT $
            mainWindow
                mPass
                dWallet
                dIsDisableButtons
                dCloudOn
                dmKey
                dResetTokens
                eRestore

    let eReEncrypt = leftmost [eNewPass, Nothing <$ eCleanOk]

    -- This delay required for preventing cancelling 'eReEncrypt' event by 'bodyWidget'.
    -- In the lag between 'eReEncrypt' and 'eNewPassDelayed' we are able to run 'reEncryptOldCache'
    -- We suppose that 2s is sufficient for re-encryption old cache
    eReEncryptDelayed <- delay 2 eReEncrypt

    -- re-encrypt old available cache with new pass
    reEncryptOldCache mPass eReEncrypt

    -- re-encrypt current cache with new pass
    reEncryptCurrentCache dTokensV3 dmKey eReEncrypt

    copiedNotification

    dSaveOnFromCache <-
        loadAppDataE Nothing isCloudOn "app-body-load-is-save-on-key" id False
    dmOldKeyBody <- loadAppDataE mPass aesKey "app-body-load-of-aes-key" id Nothing

    (dSaveWindow, dNewKeyWindow, eRestore) <-
        cloudSettingsWindow
            mPass
            dSaveOnFromCache
            dCloudStatus
            eCloudOpen
    dCloudOn <-
        holdDyn False $ leftmost $ map updated [dSaveOnFromCache, dSaveWindow]

    dResetTokens <- holdDyn False $ updated $ isNothing <$> dmOldKeyBody

    dmKey <-
        holdUniqDyn
            =<< (holdDyn Nothing $ leftmost $ map updated [dmOldKeyBody, dNewKeyWindow])

    pure eReEncryptDelayed

bodyWidget :: (MonadWidget t m) => m ()
bodyWidget = waitForScripts blank $ mdo
    mPass <- toPasswordHash <$> loadCacheValue passwordStorageKey
    (ePassOk, eCleanCache) <- case mPass of
        Just pass -> first (Just <$>) <$> enterPasswordWindow pass eCleanOk
        Nothing -> do
            ePb <- getPostBuild
            pure (Nothing <$ ePb, never)
    eCleanOk <- cleanCacheDialog eCleanCache
    dmmPass <-
        holdDyn Nothing $ Just <$> leftmost [ePassOk, Nothing <$ eCleanOk, eNewPass]
    eNewPass <- switchHoldDyn dmmPass $ \case
        Nothing -> pure never
        Just pass -> bodyContentWidget pass
    jQueryWidget
