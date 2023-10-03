module ENCOINS.DAO.Body (bodyWidget) where

import           Control.Monad                      (void)
import           Data.Functor                       (($>))
import           Reflex.Dom
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.IntMap.Strict              (toDescList)


import           Backend.Status                     (isStatusBusy, Status(..))
import           Backend.Wallet                     (Wallet (..), walletsSupportedInDAO, networkConfig, NetworkConfig(..))
import           ENCOINS.App.Widgets.Basic          (waitForScripts, elementResultJS)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.Common.Widgets.Advanced    (foldDynamicAny)
import           ENCOINS.Common.Widgets.Basic       (notification, otherStatus)
import           ENCOINS.DAO.Polls
import           ENCOINS.DAO.Widgets.Navbar         (navbarWidget, Dao (..))
import           ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import           ENCOINS.DAO.Widgets.PollWidget
import           ENCOINS.Website.Widgets.Basic      (section, container)
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Common.Events              (logEvent)


bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eDao <- navbarWidget dWallet dIsDisableButtons
  let eConnectOpen = void $ ffilter (==Connect) eDao
  let eDelegate = void $ ffilter (==Delegate) eDao

  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen
  delegateWindow eDelegate dWallet

  eVoteStatus <- voteStatus
  logEvent "Vote status" eVoteStatus
  dVoteStatus <- holdDyn Ready eVoteStatus

  eDelegateStatus <- delegateStatus
  logEvent "Delegate Status" eDelegateStatus
  dDelegateStatus <- holdDyn Ready eDelegateStatus

  eWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated eWalletLoad

  let dIsDisableButtons = foldDynamicAny
        [ isStatusBusy <$> dDelegateStatus
        , isStatusBusy <$> dVoteStatus
        ]

  let eVoteStatusT = ("Vote status: " <>) . toText <$> eVoteStatus
  let eDelegateStatusT = ("Delegate status: " <>) . toText <$> eDelegateStatus
  let wrongNetworkText = "Wrong network! Please switch to " <> toText (dao networkConfig)
  let eNetworkStatus = ffilter
        (\w -> walletNetworkId w /= dao networkConfig)
        eLoadedWallet
  dNotification <- holdDyn T.empty $ leftmost
    [ eVoteStatusT
    , eDelegateStatusT
    , wrongNetworkText <$ eNetworkStatus
    ]
  notification dNotification

  nowTime <- liftIO getCurrentTime

  let (archivedPolls, activePolls) = poolsActiveAndArchived nowTime

  section "" "" $ do
    container "" $ elAttr "div" ("class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);") $ text "Active poll"
    mapM_ (pollWidget dWallet dIsDisableButtons . snd) $ toDescList activePolls
    blank

  section "" "" $ do
    container "" $ elAttr "div" ("class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);") $ text "Concluded polls"
    mapM_ (pollCompletedWidget . snd) $ toDescList archivedPolls

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e

voteStatus :: MonadWidget t m => m (Event t Status)
voteStatus = do
  eConstruct <- updated <$> elementResultJS "VoteCreateNewTx" id
  eSign <- updated <$> elementResultJS "VoteSignTx" id
  eSubmit <- updated <$> elementResultJS "VoteSubmitTx" id
  eSubmitted <- updated <$> elementResultJS "VoteSubmitedTx" id
  eReady <- updated <$> elementResultJS "VoteReadyTx" id
  eErr <- updated <$> elementResultJS "VoteError" id
  eErrStatus <- otherStatus eErr
  pure $ leftmost [
          eErrStatus
        , Ready                            <$ eReady
        , Constructing                     <$ eConstruct
        , Signing                          <$ eSign
        , Submitting                       <$ eSubmit
        , Submitted                        <$ eSubmitted
        ]

delegateStatus :: MonadWidget t m
  => m (Event t Status)
delegateStatus = do
  eConstruct <- updated <$> elementResultJS "DelegateCreateNewTx" id
  eSign <- updated <$> elementResultJS "DelegateSignTx" id
  eSubmit <- updated <$> elementResultJS "DelegateSubmitTx" id
  eSubmitted <- updated <$> elementResultJS "DelegateSubmitedTx" id
  eReady <- updated <$> elementResultJS "DelegateReadyTx" id
  eErr <- updated <$> elementResultJS "DelegateError" id
  eErrStatus <- otherStatus eErr
  pure $ leftmost [
          eErrStatus
        , Ready                            <$ eReady
        , Constructing                     <$ eConstruct
        , Signing                          <$ eSign
        , Submitting                       <$ eSubmit
        , Submitted                        <$ eSubmitted
        ]