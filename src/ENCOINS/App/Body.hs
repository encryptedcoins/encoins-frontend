module ENCOINS.App.Body (bodyWidget) where

import           Control.Monad                      ((<=<))
import           Data.Aeson                         (encode)
import           Data.Bifunctor                     (first)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Functor                       (($>), (<&>))
import           Data.Text.Encoding                 (decodeUtf8)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.App.Widgets.MainWindow     (mainWindow)
import           ENCOINS.App.Widgets.Navbar         (navbarWidget)
import           ENCOINS.App.Widgets.PasswordWindow
import           ENCOINS.App.Widgets.WelcomeWindow  (welcomeWindow, welcomeWallet, welcomeWindowWalletStorageKey)
import           ENCOINS.Common.Widgets.Advanced    (copiedNotification, noRelayNotification)
import           JS.App                             (loadHashedPassword)
import           JS.Website                         (saveJSON)

bodyContentWidget :: MonadWidget t m => Maybe PasswordRaw -> m (Event t (Maybe PasswordRaw))
bodyContentWidget mpass = mdo
  (eSettingsOpen, eConnectOpen) <- navbarWidget dWallet
  dWallet <- connectWindow eConnectOpen
  (eNewPass, eResetPass) <- passwordSettingsWindow eSettingsOpen
  eResetOk <- resetPasswordDialog eResetPass

  welcomeWindow welcomeWindowWalletStorageKey welcomeWallet

  divClass "section-app section-app-empty wf-section" blank

  dSecrets <- mainWindow mpass dWallet
  performEvent_ (reencryptEncoins <$> attachPromptlyDyn dSecrets (leftmost
    [eNewPass, Nothing <$ eResetOk]))

  copiedNotification
  noRelayNotification

  return $ leftmost [Nothing <$ eResetOk, eNewPass]
  where
    reencryptEncoins (d, mNewPass) = saveJSON (getPassRaw <$> mNewPass) "encoins"
      . decodeUtf8 .  toStrict . encode $ d

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  mPass <- fmap PasswordHash <$> loadHashedPassword passwordSotrageKey
  (ePassOk, eReset) <- case mPass of
    Just pass -> first (Just <$>) <$> enterPasswordWindow pass eResetOk
    _ -> do
      ePb <- getPostBuild
      return (Nothing <$ ePb, never)
  eResetOk <- resetPasswordDialog eReset
  dmmPass <- holdDyn Nothing $ fmap Just $ leftmost [ePassOk, Nothing <$ eResetOk, eNewPass]
  eNewPass <- switchHold never <=< dyn $ dmmPass <&> \case
    Nothing -> pure never
    Just mpass -> bodyContentWidget mpass

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e
  
