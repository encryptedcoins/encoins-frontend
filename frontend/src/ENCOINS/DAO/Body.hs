{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Body
    ( bodyWidget
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IntMap.Strict (toDescList)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Reflex.Dom

import Backend.Wallet (walletsSupportedInDAO)
import Common.Events
import Common.Reflex.Dom.Extra (textLocale)
import ENCOINS.Common.Cache (locale)
import ENCOINS.Common.ConnectWindow (connectWindow)
import ENCOINS.Common.Widgets.Advanced (waitForScripts)
import ENCOINS.Common.Widgets.Basic (notification)
import ENCOINS.Common.Widgets.JQuery (jQueryWidget)
import ENCOINS.Common.Widgets.Locale (cacheLocale, decodeLocale)
import ENCOINS.Common.Widgets.MoreMenu
    ( WindowMoreMenuClass (..)
    , moreMenuWindow
    )
import ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import ENCOINS.DAO.Widgets.DelegateWindow.RelayTable (fetchRelayNames)
import ENCOINS.DAO.Widgets.Navbar (Dao (..), navbarWidget)
import ENCOINS.DAO.Widgets.Poll.Polls
import ENCOINS.DAO.Widgets.PollWidget
import ENCOINS.DAO.Widgets.StatusWidget
import ENCOINS.Website.Widgets.Basic (container, section)
import qualified I18n.Dao as I18n
import I18n.I18n (App)
import I18n.Reflex.I18n (Locale, runLocalize)
import JS.App (loadCacheValue)

bodyWidget :: (MonadWidget t m) => m ()
bodyWidget = waitForScripts "walletAPI" "js/ENCOINS.js" blank $ mdo
    localeInCache <- decodeLocale <$> loadCacheValue locale
    dLocaleNew <- runLocalize dLocale $ bodyContentWidget localeInCache
    logDyn "bodyWidget: dLocaleNew" dLocaleNew
    dLocale <- holdUniqDyn =<< holdDyn localeInCache (updated dLocaleNew)
    jQueryWidget

bodyContentWidget :: (App t m) => Locale -> m (Dynamic t Locale)
bodyContentWidget currentLocale = mdo
    eFireNames <- newEvent
    dRelayNames <- fetchRelayNames eFireNames
    (eDao, dLocaleNew) <-
        navbarWidget dWallet dIsDisableButtons dIsDisableConnectButton currentLocale

    let eMoreMenuOpen = void $ ffilter (== MoreMenu) eDao
    let moreMenuClass =
            WindowMoreMenuClass
                "common-MoreMenu_Window"
                "common-MoreMenu_LinkContainer"
                "common-MoreMenu_Link"
    moreMenuWindow moreMenuClass eMoreMenuOpen

    let eConnectOpen = void $ ffilter (== Connect) eDao
    dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

    let eDelegate = void $ ffilter (== Delegate) eDao
    delegateWindow eDelegate dWallet dRelayNames

    (dIsDisableButtons, dIsDisableConnectButton, dNotification) <-
        handleStatus dWallet
    notification dNotification

    (archivedPolls, activePolls) <- poolsActiveAndArchived <$> liftIO getCurrentTime

    section "" "" $ do
        container "" $
            elAttr "div" pollAttr $
                textLocale I18n.ActivePoll
        mapM_ (pollWidget dWallet dIsDisableButtons . snd) $ toDescList activePolls
        blank

    section "" "" $ do
        container "" $
            elAttr "div" pollAttr $
                textLocale I18n.ConcludedPolls
        mapM_ (pollCompletedWidget . snd) $ toDescList archivedPolls

    logDyn "bodyContentWidget: dLocaleNew" dLocaleNew
    dLocaleCashed <- cacheLocale dLocaleNew

    pure dLocaleCashed

pollAttr :: Map Text Text
pollAttr =
    "class" =: "h5"
        <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"
