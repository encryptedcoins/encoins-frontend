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
                                                     isReadyOrNoError,
                                                     isTxProcess,
                                                     isTxProcessOrCriticalError,
                                                     isWalletError)
import           Backend.Wallet                     (LucidConfig (..),
                                                     Wallet (..),
                                                     WalletName (..), fromJS,
                                                     hasToken, lucidConfigDao,
                                                     walletsSupportedInDAO)
import           Config.Config                      (NetworkConfig (dao),
                                                     networkConfig)
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
import           ENCOINS.Website.Widgets.Basic      (container, section)

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget
  jQueryWidget

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eDao <- navbarWidget dWallet dIsDisableButtons dIsDisableConnectButton

  let eConnectOpen = void $ ffilter (==Connect) eDao
  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

  let eDelegate = void $ ffilter (==Delegate) eDao
  delegateWindow eDelegate dWallet

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
  eSubmitted <- updated <$> elementResultJS "DelegateSubmittedTx" id
  eReady <- updated <$> elementResultJS "DelegateReadyTx" id
  -- Wait one minute until the changes take place.
  eReadyDelayed <- delay 60 eReady
  eErr <- updated <$> elementResultJS "DelegateError" id
  eErrStatus <- otherStatus eErr
  pure $ leftmost [
          eErrStatus
        , Ready                            <$ eReadyDelayed
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
  -> m (Dynamic t Bool, Dynamic t Status)
handleInvalidNetwork dWallet = do
  dWalletLoad <- elementResultJS "EndWalletLoad" id
  let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
  let eUnexpectedNetworkB = fmap
        (\w -> walletNetworkId w /= dao networkConfig)
        eLoadedWallet
  dUnexpectedNetworkB <- holdDyn False eUnexpectedNetworkB
  let mkNetworkMessage isInvalidNetwork message =
        case (isInvalidNetwork, message) of
          (True,_)         -> Just $ WalletNetworkError unexpectedNetwork
          (False, NoError) -> Nothing
          (False, _)       -> Just NoError
  dUnexpectedNetworkS <- foldDynMaybe mkNetworkMessage Ready eUnexpectedNetworkB
  pure (dUnexpectedNetworkB, dUnexpectedNetworkS)

handleWalletNone :: MonadWidget t m
  => m (Dynamic t Bool, Dynamic t Status)
handleWalletNone = do
  eWalletName <- updated <$> elementResultJS "daoWalletNameNotConnected" fromJS
  dWalletMessage <-
    foldDyn (\w _ -> if w == None
        then (True, WalletError "Wallet is not connected!")
        else (False, NoError))
    (False, NoError)
    eWalletName
  dWalletMessageUniq <- holdUniqDyn dWalletMessage
  pure $ splitDynPure dWalletMessageUniq

handleEncToken :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t Bool, Event t Status)
handleEncToken dWallet = do
  let LucidConfig _ _ encPolicy encName = lucidConfigDao
  let eWalletConnected = ffilter (\w -> walletName w /= None) $ updated dWallet
  let eHasNotToken = not . hasToken encPolicy encName <$> eWalletConnected
  let eHasNotTokenStatus = bool
        NoError (WalletError "No ENCS tokens to delegate!") <$> eHasNotToken
  dHasNotToken <- holdDyn False eHasNotToken
  pure (dHasNotToken, eHasNotTokenStatus)

handleStatus :: MonadWidget t m
  => Dynamic t Wallet
  -> m (Dynamic t Bool, Dynamic t Bool, Dynamic t Text)
handleStatus dWallet = do
  eVoteStatus <- voteStatus
  dVoteStatus <- holdDyn Ready eVoteStatus

  eDelegateStatus <- delegateStatus
  dDelegateStatus <- holdDyn Ready eDelegateStatus

  (dUnexpectedNetworkB, dUnexpectedNetworkT) <- handleInvalidNetwork dWallet
  (dWalletNotConnectedB, dWalletNotConnectedT) <- handleWalletNone
  eWalletError <- walletError
  dWalletError <- holdDyn False $ isWalletError <$> eWalletError
  (dHasNotToken, eHasNotTokenStatus) <- handleEncToken dWallet

  let dIsDisableButtons = foldDynamicAny
        [ isTxProcess <$> dDelegateStatus
        , isTxProcess <$> dVoteStatus
        , dUnexpectedNetworkB
        , dWalletNotConnectedB
        , dWalletError
        , dHasNotToken
        ]
  let dIsDisableConnectButton = foldDynamicAny
        [ isTxProcess <$> dDelegateStatus
        , isTxProcess <$> dVoteStatus
        , dUnexpectedNetworkB
        ]

  let flatStatus (t, s) = bool (t <> column <> space <> toText s) T.empty $ isReadyOrNoError s

  let currentStatus = leftmost
        [ ("Vote status",) <$> eVoteStatus
        , ("Delegate status",) <$> eDelegateStatus
        , ("",) <$> updated dUnexpectedNetworkT
        , ("",) <$> updated dWalletNotConnectedT
        , ("Wallet",) <$> eWalletError
        , ("Wallet",) <$> eHasNotTokenStatus
        ]
  logEvent "Current status" currentStatus
  dNotification <- foldDyn processStatus ("", Ready) currentStatus
  logDyn "dNotification" dNotification

  pure (dIsDisableButtons, dIsDisableConnectButton, flatStatus <$> dNotification)

pollAttr :: Map Text Text
pollAttr =
  "class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"

processStatus :: (Text, Status) -> (Text, Status) -> (Text, Status)
processStatus new old =
  case ( isTxProcessOrCriticalError $ snd old
       , isTxProcessOrCriticalError (snd new) || isReady (snd new)
       ) of
    (True, False) -> old
    _             -> new
