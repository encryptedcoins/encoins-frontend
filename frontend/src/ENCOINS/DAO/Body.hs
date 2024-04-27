{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Body (bodyWidget) where

import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.IntMap.Strict                 (toDescList)
import           Data.Map                           (Map)
import           Data.Text                          (Text)
import           Data.Time                          (getCurrentTime)
import           Reflex.Dom

import           Backend.Wallet                     (walletsSupportedInDAO)
import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Basic       (notification)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           ENCOINS.Common.Widgets.MoreMenu    (WindowMoreMenuClass (..),
                                                     moreMenuWindow)
import           ENCOINS.DAO.Polls
import           ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import           ENCOINS.DAO.Widgets.Navbar         (Dao (..), navbarWidget)
import           ENCOINS.DAO.Widgets.PollWidget
import           ENCOINS.DAO.Widgets.RelayTable     (fetchRelayNames)
import           ENCOINS.DAO.Widgets.StatusWidget
import           ENCOINS.Website.Widgets.Basic      (container, section)

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget
  jQueryWidget

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eFireNames <- newEvent
  dRelayNames <- fetchRelayNames eFireNames
  eDao <- navbarWidget dWallet dIsDisableButtons dIsDisableConnectButton

  let eMoreMenuOpen = void $ ffilter (==MoreMenu) eDao
  let moreMenuClass = WindowMoreMenuClass
        "common-MoreMenu_Window"
        "common-MoreMenu_LinkContainer"
        "common-MoreMenu_Link"
  moreMenuWindow moreMenuClass eMoreMenuOpen

  let eConnectOpen = void $ ffilter (==Connect) eDao
  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

  let eDelegate = void $ ffilter (==Delegate) eDao
  delegateWindow eDelegate dWallet dRelayNames

  (dIsDisableButtons, dIsDisableConnectButton, dNotification) <- handleStatus dWallet
  notification dNotification

  (archivedPolls, activePolls) <- poolsActiveAndArchived <$> liftIO getCurrentTime

  section "" "" $ do
    container ""
      $ elAttr "div" pollAttr
      $ text "Active poll"
    mapM_ (pollWidget dWallet dIsDisableButtons . snd) $ toDescList activePolls
    blank

  section "" "" $ do
    container ""
      $ elAttr "div" pollAttr
      $ text "Concluded polls"
    mapM_ (pollCompletedWidget . snd) $ toDescList archivedPolls


pollAttr :: Map Text Text
pollAttr =
  "class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"
