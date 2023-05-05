module ENCOINS.App.Body (bodyWidget) where

import           Data.Functor                       (($>))
import           Data.Bifunctor                     (first)
import           Reflex.Dom

import           ENCOINS.App.Widgets
import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWindow, welcomeWallet, welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification)

bodyContentWidget :: MonadWidget t m => Maybe PasswordRaw -> m ()
bodyContentWidget mpass = mdo
  (eSettingsOpen, eConnectOpen) <- navbarWidget dWallet
  dWallet <- connectWindow eConnectOpen
  eResetPass <- passwordSettingsWindow eSettingsOpen
  _ <- resetPasswordDialog eResetPass

  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  mainWindow mpass dWallet

  -- divClass "section-app section-app-empty wf-section" blank

  -- divClass "footer wf-section" $ divClass "container-footer" $ divClass "div-our-resourses" $ ourResourses "50px"

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  mPass <- (>>= checkPasswordHash) <$> loadTextFromStorage passwordSotrageKey
  (ePassOk, eReset) <- case mPass of
    Just pass -> first (Nothing <$) <$> enterPasswordWindow pass eResetOk
    _ -> do
      ePb <- getPostBuild
      return (Nothing <$ ePb, never)
  eResetOk <- resetPasswordDialog eReset
  widgetHold_ blank (bodyContentWidget <$> leftmost [ePassOk, Nothing <$ eResetOk])

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e
  copiedNotification
