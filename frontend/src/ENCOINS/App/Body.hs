{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Body
    ( bodyWidget
    ) where

import Data.Align (align)
import Data.Maybe (isNothing)
import Reflex.Dom

import Backend.Protocol.StrongTypes (toPasswordHash)
import Backend.Protocol.Types (PasswordRaw (..))
import Backend.Status (AppStatus (..))
import Backend.Wallet (Wallet (walletName), walletsSupportedInApp)
import Common.Events
import Common.Reflex.Extra (switchHoldDyn)
import ENCOINS.App.Widgets.CloudWindow (cloudSettingsWindow)
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
    , loadAppDataE
    , passwordStorageKey
    )
import ENCOINS.Common.ConnectWindow (connectWindow)
import ENCOINS.Common.Widgets.Advanced (viewCopiedNotification, waitForScripts)
import ENCOINS.Common.Widgets.Basic (notification)
import ENCOINS.Common.Widgets.JQuery (jQueryWidget)
import ENCOINS.Common.Widgets.MoreMenu
    ( WindowMoreMenuClass (..)
    , moreMenuWindow
    )
import I18n.Reflex.I18n
import JS.App (loadCacheValue)

bodyContentWidget ::
    (App t m) =>
    Maybe PasswordRaw
    -> m (Event t (Maybe PasswordRaw), Dynamic t Locale)
bodyContentWidget mPass = mdo
    (ePassOpen, eConnectOpen, eCloudOpen, eMoreMenuOpen, dLocale) <-
        navbarWidget
            dWallet
            dIsBlockAllButtons
            mPass
            dCloudOn
            dCloudStatus
            dIsBlockConnectButton

    let moreMenuClass =
            WindowMoreMenuClass
                "common-MoreMenu_Window"
                "common-MoreMenu_LinkContainer"
                "common-MoreMenu_Link"
    moreMenuWindow moreMenuClass eMoreMenuOpen

    (dStatusT, dIsBlockAllButtons, dCloudStatus, dIsBlockConnectButton) <-
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
                dIsBlockAllButtons
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

    viewCopiedNotification

    dSaveOnFromCache <-
        loadAppDataE Nothing isCloudOn "app-body-load-is-save-on-key" id False
    dmOldKeyBody <- loadAppDataE mPass aesKey "app-body-load-of-aes-key" id Nothing

    (dSaveWindow, dNewKeyWindow, eRestore) <-
        cloudSettingsWindow
            mPass
            (walletName <$> dWallet)
            dSaveOnFromCache
            dCloudStatus
            eCloudOpen
    dCloudOn <-
        holdDyn False $ leftmost $ map updated [dSaveOnFromCache, dSaveWindow]

    dResetTokens <- holdDyn False $ updated $ isNothing <$> dmOldKeyBody

    dmKey <-
        holdUniqDyn
            =<< (holdDyn Nothing $ leftmost $ map updated [dmOldKeyBody, dNewKeyWindow])

    pure (eReEncryptDelayed, dLocale)

bodyWidget :: (MonadWidget t m) => m ()
bodyWidget = waitForScripts blank $ mdo
    mPass <- toPasswordHash <$> loadCacheValue passwordStorageKey
    (ePassOk, eCleanCache) <- case mPass of
        Just pass -> do
            (passRaw, ev) <- runLocalize dLocale $ enterPasswordWindow pass eCleanOk
            pure (Just <$> passRaw, ev)
        Nothing -> do
            ePb <- getPostBuild
            pure (Nothing <$ ePb, never)
    eCleanOk <- runLocalize dLocale $ cleanCacheDialog eCleanCache
    dmmPass <-
        holdDyn Nothing $ Just <$> leftmost [ePassOk, Nothing <$ eCleanOk, eNewPass]
    eThesePassLocale <- switchHoldDyn dmmPass $ \case
        Nothing -> pure $ align never never
        Just pass -> do
            (ePass, dLocale') <- runLocalize dLocale $ bodyContentWidget pass
            pure $ align ePass $ updated dLocale'
    let (eNewPass, eLocale) = fanThese eThesePassLocale
    logEvent "bodyWidget: eLocale" eLocale
    dLocale <- holdUniqDyn =<< holdDyn Locale_EN eLocale 
    jQueryWidget
