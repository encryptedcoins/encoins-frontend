module ENCOINS.DAO.Body (bodyWidget) where

import           Data.Bool                          (bool)
import           Data.Functor                       (($>))
import           Reflex.Dom

import           Backend.Wallet                     (Wallet (..), WalletName (..), walletsSupportedInDAO)
import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.Common.Widgets.Advanced    (wrongNetworkNotification)
import           ENCOINS.DAO.Polls
import           ENCOINS.DAO.Widgets.Navbar         (navbarWidget, Dao (..))
import           ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import           ENCOINS.DAO.Widgets.PollWidget
import           ENCOINS.Website.Widgets.Basic      (section, container)
import           JS.Website                         (setElementStyle)
import            ENCOINS.Common.Events (logEvent)
import            Control.Monad (void)

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eDao <- navbarWidget dWallet dStatus
  logEvent "eDao" eDao
  let eConnectOpen = void $ ffilter (==Connect) eDao
  let eDelegate = void $ ffilter (==Delegate) eDao

  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen
  dStatus <- delegateWindow eDelegate dWallet

  section "" "" $ do
    container "" $ elAttr "div" ("class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);") $ text "Active poll"
    pollWidget poll5 dWallet
    blank

  section "" "" $ do
    container "" $ elAttr "div" ("class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);") $ text "Concluded polls"
    pollCompletedWidget poll4
    pollCompletedWidget poll3
    pollCompletedWidget poll2
    pollCompletedWidget poll1

  wrongNetworkNotification "Mainnet"
  let eNotificationStyleChange = bool "flex" "none" . (\w -> walletNetworkId w == "1" || walletName w == None) <$> updated dWallet
  performEvent_ $ setElementStyle "bottom-notification-network" "display" <$> eNotificationStyleChange

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e