module ENCOINS.DAO.Body (bodyWidget) where

import           Control.Monad                      (void)
import           Reflex.Dom
import qualified Data.Text as T
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.Time (getCurrentTime)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.IntMap.Strict              (toDescList)

import           Backend.Status                     (isStatusBusy, Status(..))
import           Backend.Wallet                     (Wallet (..), walletsSupportedInDAO, networkConfig, NetworkConfig(..))
import           ENCOINS.App.Widgets.Basic          (waitForScripts, elementResultJS)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.Common.Widgets.Advanced    (foldDynamicAny)
import           ENCOINS.Common.Widgets.Basic       (notification, otherStatus, space)
import           ENCOINS.DAO.Polls
import           ENCOINS.DAO.Widgets.Navbar         (navbarWidget, Dao (..))
import           ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import           ENCOINS.DAO.Widgets.PollWidget
import           ENCOINS.Website.Widgets.Basic      (section, container)
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Common.Events              (logEvent)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)


bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget
  jQueryWidget

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eDao <- navbarWidget dWallet dIsDisableButtons

  let eConnectOpen = void $ ffilter (==Connect) eDao
  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

  let eDelegate = void $ ffilter (==Delegate) eDao
  delegateWindow eDelegate dWallet

  (dIsDisableButtons, dNotification) <- handleStatus dWallet
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

unexpectedNetwork :: Text
unexpectedNetwork =
           "Unexpected network! Please switch the wallet to"
        <> space
        <> toText (dao networkConfig)
        <> space
        <> "mode."

handleInvalidNetwork :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t Bool, Dynamic t Text)
handleInvalidNetwork dWallet = do
  eWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated eWalletLoad

  let eUnexpectedNetworkB = fmap
        (\w -> walletNetworkId w /= dao networkConfig)
        eLoadedWallet
  -- logEvent "eUnexpectedNetworkB" eUnexpectedNetworkB
  dUnexpectedNetworkB <- holdDyn False eUnexpectedNetworkB

  let mkNetworkMessage isInvalidNetwork message =
        case (isInvalidNetwork, message) of
          (True,_) -> Just unexpectedNetwork
          (False, "") -> Nothing
          (False, _) -> Just ""
  dUnexpectedNetworkT <- foldDynMaybe mkNetworkMessage "" eUnexpectedNetworkB

  pure (dUnexpectedNetworkB, dUnexpectedNetworkT)

handleStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t Bool, Dynamic t Text)
handleStatus dWallet = do
  eVoteStatus <- voteStatus
  logEvent "Vote status" eVoteStatus
  dVoteStatus <- holdDyn Ready eVoteStatus

  eDelegateStatus <- delegateStatus
  logEvent "Delegate Status" eDelegateStatus
  dDelegateStatus <- holdDyn Ready eDelegateStatus

  (dUnexpectedNetworkB, dUnexpectedNetworkT) <- handleInvalidNetwork dWallet

  let dIsDisableButtons = foldDynamicAny
        [ isStatusBusy <$> dDelegateStatus
        , isStatusBusy <$> dVoteStatus
        , dUnexpectedNetworkB
        ]

  let eVoteStatusT = ("Vote status: " <>) . toText <$> eVoteStatus
  let eDelegateStatusT = ("Delegate status: " <>) . toText <$> eDelegateStatus

  dNotification <- holdDyn T.empty $ leftmost
    [ eVoteStatusT
    , eDelegateStatusT
    , updated dUnexpectedNetworkT
    ]

  pure (dIsDisableButtons, dNotification)

pollAttr :: Map Text Text
pollAttr =
  "class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"
