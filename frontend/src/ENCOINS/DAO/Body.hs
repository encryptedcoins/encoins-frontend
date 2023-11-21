{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Body (bodyWidget) where

import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Bool                          (bool)
import           Data.IntMap.Strict                 (toDescList)
import           Data.Map                           (Map)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time                          (getCurrentTime)
import           Reflex.Dom

import           Backend.Status                     (Status (..), isReady,
                                                     isTxProcess, isWalletError)
import           Backend.Wallet                     (NetworkConfig (..),
                                                     Wallet (..),
                                                     WalletName (..), fromJS,
                                                     networkConfig,
                                                     walletsSupportedInDAO)
import           ENCOINS.App.Widgets.Basic          (elementResultJS,
                                                     waitForScripts,
                                                     walletError)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Common.Widgets.Advanced    (foldDynamicAny)
import           ENCOINS.Common.Widgets.Basic       (column, notification,
                                                     otherStatus, space)
import           ENCOINS.Common.Widgets.JQuery      (jQueryWidget)
import           ENCOINS.DAO.Polls
import           ENCOINS.DAO.Widgets.DelegateWindow (delegateWindow)
import           ENCOINS.DAO.Widgets.Navbar         (Dao (..), navbarWidget)
import           ENCOINS.DAO.Widgets.PollWidget
import           ENCOINS.DAO.Widgets.RelayTable     (fetchRelayTable)
import           ENCOINS.Website.Widgets.Basic      (container, section)

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget
  jQueryWidget

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eDao <- navbarWidget dWallet dIsDisableButtons dIsDisableButtonsConnect

  let eConnectOpen = void $ ffilter (==Connect) eDao
  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

  let eDelegate = void $ ffilter (==Delegate) eDao
  eDelay <- postDelay 0.05
  dRelays <- holdDyn [] =<< fetchRelayTable eDelay
  delegateWindow eDelegate dWallet dRelays

  (dIsDisableButtons, dIsDisableButtonsConnect, dNotification) <- handleStatus dWallet
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
  eSubmitted <- updated <$> elementResultJS "VoteSubmittedTx" id
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
  dWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
  let eUnexpectedNetworkB = fmap
        (\w -> walletNetworkId w /= dao networkConfig)
        eLoadedWallet
  dUnexpectedNetworkB <- holdDyn False eUnexpectedNetworkB
  let mkNetworkMessage isInvalidNetwork message =
        case (isInvalidNetwork, message) of
          (True,_)    -> Just unexpectedNetwork
          (False, "") -> Nothing
          (False, _)  -> Just ""
  dUnexpectedNetworkT <- foldDynMaybe mkNetworkMessage "" eUnexpectedNetworkB
  pure (dUnexpectedNetworkB, dUnexpectedNetworkT)

handleWalletNone :: MonadWidget t m
  => m (Dynamic t Bool, Dynamic t Text)
handleWalletNone = do
  eWalletName <- updated <$> elementResultJS "daoWalletNameNotConnected" fromJS
  dWalletMessage <-
    foldDyn (\w _ -> if w == None then (True, "Wallet is not connected!") else (False, ""))
    (False, "")
    eWalletName
  let (dWalletNotConnectedB, dWalletNotConnectedT) = splitDynPure dWalletMessage
  pure (dWalletNotConnectedB, dWalletNotConnectedT)

handleStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t Bool, Dynamic t Bool, Dynamic t Text)
handleStatus dWallet = do
  eVoteStatus <- voteStatus
  logEvent "Vote status" eVoteStatus
  dVoteStatus <- holdDyn Ready eVoteStatus

  eDelegateStatus <- delegateStatus
  logEvent "Delegate Status" eDelegateStatus
  dDelegateStatus <- holdDyn Ready eDelegateStatus

  (dUnexpectedNetworkB, dUnexpectedNetworkT) <- handleInvalidNetwork dWallet
  (dWalletNotConnectedB, dWalletNotConnectedT) <- handleWalletNone
  eWalletError <- walletError
  dWalletError <- holdDyn False $ isWalletError <$> eWalletError

  let dIsDisableButtons = foldDynamicAny
        [ isTxProcess <$> dDelegateStatus
        , isTxProcess <$> dVoteStatus
        , dUnexpectedNetworkB
        , dWalletNotConnectedB
        , dWalletError
        ]
  let dIsDisableButtonsConnect = foldDynamicAny
        [ isTxProcess <$> dDelegateStatus
        , isTxProcess <$> dVoteStatus
        , dUnexpectedNetworkB
        ]

  let flatStatus t s = bool (t <> column <> space <> toText s) T.empty $ isReady s

  dNotification <- holdDyn T.empty $ leftmost
    [ flatStatus "Vote status" <$> eVoteStatus
    , flatStatus "Delegate status" <$> eDelegateStatus
    , updated dUnexpectedNetworkT
    , updated dWalletNotConnectedT
    , flatStatus "Wallet" <$> eWalletError
    ]

  pure (dIsDisableButtons, dIsDisableButtonsConnect, dNotification)

pollAttr :: Map Text Text
pollAttr =
  "class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"
