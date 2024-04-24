module ENCOINS.DAO.Widgets.StatusWidget where

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

import Backend.Status
    ( Status (..)
    , isBuffer
    , isReadyOrNoError
    , isStatusWantBlockButtons
    , isTxProcess
    , isWalletError
    )
import Backend.Utility (column, space, toText)
import Backend.Wallet
    ( LucidConfig (..)
    , Wallet (..)
    , WalletName (..)
    , fromJS
    , hasToken
    , lucidConfigDao
    )
import Config.Config (NetworkConfig (dao), networkConfig)
import ENCOINS.App.Widgets.Basic (elementResultJS, walletError)
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Advanced (foldDynamicAny)

handleStatus ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t Bool, Dynamic t Bool, Dynamic t Text)
handleStatus dWallet = do
    eVoteStatus <- voteStatus
    dVoteStatus <- holdDyn Ready eVoteStatus

    eDelegateStatus <- delegateStatus
    dDelegateStatus <- holdDyn Ready eDelegateStatus

    (dUnexpectedNetworkB, dUnexpectedNetworkS) <- handleInvalidNetwork dWallet
    (dWalletNotConnectedB, dWalletNotConnectedS) <- handleWalletNone

    eWalletError <- walletError
    dWalletError <- holdDyn False $ isWalletError <$> eWalletError

    (dHasNotToken, eHasNotTokenStatus) <- handleEncToken dWallet

    let dIsDisableButtons =
            foldDynamicAny
                [ isTxProcess <$> dDelegateStatus
                , isTxProcess <$> dVoteStatus
                , dUnexpectedNetworkB
                , dWalletNotConnectedB
                , dWalletError
                , dHasNotToken
                ]
    let dIsDisableConnectButton =
            foldDynamicAny
                [ isTxProcess <$> dDelegateStatus
                , isTxProcess <$> dVoteStatus
                , dUnexpectedNetworkB
                ]

    let flatStatus (t, s) = bool (t <> column <> space <> toText s) T.empty $ isReadyOrNoError s

    let currentStatus =
            leftmost
                [ ("Vote status",) <$> eVoteStatus
                , ("Delegate status",) <$> eDelegateStatus
                , ("Wallet",) <$> updated dUnexpectedNetworkS
                , ("Wallet",) <$> updated dWalletNotConnectedS
                , ("Wallet",) <$> eWalletError
                , ("Wallet",) <$> eHasNotTokenStatus
                ]
    logEvent "Current status" currentStatus
    dNotification <- foldDyn processStatus ("", Ready) currentStatus

    pure (dIsDisableButtons, dIsDisableConnectButton, flatStatus <$> dNotification)

voteStatus :: (MonadWidget t m) => m (Event t Status)
voteStatus = do
    eConstruct <- updated <$> elementResultJS "VoteCreateNewTx" id
    eSign <- updated <$> elementResultJS "VoteSignTx" id
    eSubmit <- updated <$> elementResultJS "VoteSubmitTx" id
    eSubmitted <- updated <$> elementResultJS "VoteSubmittedTx" id
    eReady <- updated <$> elementResultJS "VoteReadyTx" id
    eErr <- updated <$> elementResultJS "VoteError" id
    pure $
        leftmost
            [ WalletError <$> eErr
            , Submitted <$ eSubmitted
            , Submitting <$ eSubmit
            , Signing <$ eSign
            , Constructing <$ eConstruct
            , Ready <$ eReady
            ]

delegateStatus ::
    (MonadWidget t m) =>
    m (Event t Status)
delegateStatus = do
    eConstruct <- updated <$> elementResultJS "DelegateCreateNewTx" id
    eSign <- updated <$> elementResultJS "DelegateSignTx" id
    eSubmit <- updated <$> elementResultJS "DelegateSubmitTx" id
    eSubmitted <- updated <$> elementResultJS "DelegateSubmittedTx" id
    eSuccess <- updated <$> elementResultJS "DelegateSuccessTx" id
    -- eSuccess and eReady fire at the same time usually
    -- Show eSubmitted status for waitConfirmationTime for waiting tx submitted
    -- Usually it is enough 60 seconds to submit it.
    let waitConfirmationTime = 60
    let showSuccessTime = 20
    eSuccessDelayed <- delay waitConfirmationTime eSuccess
    -- Show eSuccess status for showSuccessTime seconds
    eReady <- updated <$> elementResultJS "DelegateReadyTx" id
    eReadyDelayed <- delay (waitConfirmationTime + showSuccessTime) eReady
    eErr <- updated <$> elementResultJS "DelegateError" id
    pure $
        leftmost
            [ WalletError <$> eErr
            , Submitted <$ eSubmitted
            , Submitting <$ eSubmit
            , Signing <$ eSign
            , Constructing <$ eConstruct
            , Success <$ eSuccessDelayed
            , Ready <$ eReadyDelayed
            ]

handleInvalidNetwork ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t Bool, Dynamic t Status)
handleInvalidNetwork dWallet = do
    dWalletLoad <- elementResultJS "EndWalletLoad" id
    let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
    let eUnexpectedNetworkB =
            fmap
                (\w -> walletNetworkId w /= dao networkConfig)
                eLoadedWallet
    dUnexpectedNetworkB <- holdDyn False eUnexpectedNetworkB
    let mkNetworkMessage isInvalidNetwork message =
            case (isInvalidNetwork, message) of
                (True, _) -> Just $ WalletNetworkError unexpectedNetwork
                (False, Ready) -> Nothing
                (False, _) -> Just Ready
    dUnexpectedNetworkS <- foldDynMaybe mkNetworkMessage Ready eUnexpectedNetworkB
    pure (dUnexpectedNetworkB, dUnexpectedNetworkS)

handleWalletNone ::
    (MonadWidget t m) =>
    m (Dynamic t Bool, Dynamic t Status)
handleWalletNone = do
    eWalletName <- updated <$> elementResultJS "daoWalletNameNotConnected" fromJS
    dWalletMessage <-
        foldDyn
            ( \w _ ->
                if w == None
                    then (True, WalletError "Wallet is not connected!")
                    else (False, NoError)
            )
            (False, NoError)
            eWalletName
    dWalletMessageUniq <- holdUniqDyn dWalletMessage
    pure $ splitDynPure dWalletMessageUniq

handleEncToken ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t Bool, Event t Status)
handleEncToken dWallet = do
    let LucidConfig _ _ encPolicy encName = lucidConfigDao
    let eWalletConnected = ffilter (\w -> walletName w /= None) $ updated dWallet
    let eHasNotToken = not . hasToken encPolicy encName <$> eWalletConnected
    let eHasNotTokenStatus =
            bool
                NoError
                (WalletError "No ENCS tokens to delegate!")
                <$> eHasNotToken
    dHasNotToken <- holdDyn False eHasNotToken
    pure (dHasNotToken, eHasNotTokenStatus)

processStatus :: (Text, Status) -> (Text, Status) -> (Text, Status)
processStatus newSt oldSt =
    case ( isStatusWantBlockButtons $ snd oldSt
         , isStatusWantBlockButtons (snd newSt) || isBuffer (snd newSt)
         ) of
        (True, False) -> oldSt
        _ -> newSt

unexpectedNetwork :: Text
unexpectedNetwork =
    "Unexpected network! Please switch the wallet to"
        <> space
        <> toText (dao networkConfig)
        <> space
        <> "mode."
