module ENCOINS.DAO.Widgets.StatusWidget where

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

import Backend.Status
    ( DaoStatus (..)
    , DelegateTxStatus (..)
    , VoteTxStatus (..)
    , WalletStatus (..)
    , isDaoBuffer
    , isDaoReadyOrNoError
    , isDaoTotalBlock
    , isDelegateTxProcess
    , isVoteTxProcess
    , isWalletError
    , textDaoStatus
    )
import Backend.Utility (space, toText)
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
    dVoteStatus <- holdDyn VoteTxReady eVoteStatus

    eDelegateStatus <- delegateStatus
    dDelegateStatus <- holdDyn DelTxReady eDelegateStatus

    (dUnexpectedNetworkB, dUnexpectedNetworkS) <- handleInvalidNetwork dWallet
    (dWalletNotConnectedB, dWalletNotConnectedS) <- handleWalletNone

    eWalletError <- walletError
    dWalletError <- holdDyn False $ isWalletError <$> eWalletError

    (dHasNotToken, eHasNotTokenStatus) <- handleEncToken dWallet

    let dIsDisableButtons =
            foldDynamicAny
                [ isDelegateTxProcess <$> dDelegateStatus
                , isVoteTxProcess <$> dVoteStatus
                , dUnexpectedNetworkB
                , dWalletNotConnectedB
                , dWalletError
                , dHasNotToken
                ]
    let dIsDisableConnectButton =
            foldDynamicAny
                [ isDelegateTxProcess <$> dDelegateStatus
                , isVoteTxProcess <$> dVoteStatus
                ]

    let flatStatus s = bool (textDaoStatus s) T.empty $ isDaoReadyOrNoError s

    let currentStatus =
            leftmost
                [ VoteTx <$> eVoteStatus
                , DelegateTx <$> eDelegateStatus
                , WalletInDao <$> updated dUnexpectedNetworkS
                , WalletInDao <$> updated dWalletNotConnectedS
                , WalletInDao <$> eWalletError
                , WalletInDao <$> eHasNotTokenStatus
                ]
    logEvent "Current status" $ textDaoStatus <$> currentStatus
    dNotification <- foldDyn processStatus DaoReady currentStatus

    pure (dIsDisableButtons, dIsDisableConnectButton, flatStatus <$> dNotification)

voteStatus :: (MonadWidget t m) => m (Event t VoteTxStatus)
voteStatus = do
    eConstruct <- updated <$> elementResultJS "VoteCreateNewTx" id
    eSign <- updated <$> elementResultJS "VoteSignTx" id
    eSubmit <- updated <$> elementResultJS "VoteSubmitTx" id
    eSubmitted <- updated <$> elementResultJS "VoteSubmittedTx" id
    eReady <- updated <$> elementResultJS "VoteReadyTx" id
    eErr <- updated <$> elementResultJS "VoteError" id
    pure $
        leftmost
            [ VoteTxError <$> eErr
            , VoteTxSubmitted <$ eSubmitted
            , VoteTxSubmitting <$ eSubmit
            , VoteTxSigning <$ eSign
            , VoteTxConstructing <$ eConstruct
            , VoteTxReady <$ eReady
            ]

delegateStatus ::
    (MonadWidget t m) =>
    m (Event t DelegateTxStatus)
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
            [ DelTxError <$> eErr
            , DelTxSubmitted <$ eSubmitted
            , DelTxSubmitting <$ eSubmit
            , DelTxSigning <$ eSign
            , DelTxConstructing <$ eConstruct
            , DelTxSuccess <$ eSuccessDelayed
            , DelTxReady <$ eReadyDelayed
            ]

handleInvalidNetwork ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t Bool, Dynamic t WalletStatus)
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
                (False, WalletReady) -> Nothing
                (False, _) -> Just WalletReady
    dUnexpectedNetworkS <-
        foldDynMaybe mkNetworkMessage WalletReady eUnexpectedNetworkB
    pure (dUnexpectedNetworkB, dUnexpectedNetworkS)

handleWalletNone ::
    (MonadWidget t m) =>
    m (Dynamic t Bool, Dynamic t WalletStatus)
handleWalletNone = do
    eWalletName <- updated <$> elementResultJS "daoWalletNameNotConnected" fromJS
    dWalletMessage <-
        foldDyn
            ( \w _ ->
                if w == None
                    then (True, WalletError "Wallet is not connected!")
                    else (False, WalletReady)
            )
            (False, WalletReady)
            eWalletName
    dWalletMessageUniq <- holdUniqDyn dWalletMessage
    pure $ splitDynPure dWalletMessageUniq

handleEncToken ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t Bool, Event t WalletStatus)
handleEncToken dWallet = do
    let LucidConfig _ _ encPolicy encName = lucidConfigDao
    let eWalletConnected = ffilter (\w -> walletName w /= None) $ updated dWallet
    let eHasNotToken = not . hasToken encPolicy encName <$> eWalletConnected
    let eHasNotTokenStatus =
            bool
                WalletReady
                (WalletError "No ENCS tokens to delegate!")
                <$> eHasNotToken
    dHasNotToken <- holdDyn False eHasNotToken
    pure (dHasNotToken, eHasNotTokenStatus)

processStatus :: DaoStatus -> DaoStatus -> DaoStatus
processStatus newSt oldSt =
    case ( isDaoTotalBlock oldSt
         , isDaoTotalBlock newSt || isDaoBuffer newSt
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
