module ENCOINS.DAO.Widgets.StatusWidget where

import Data.Bool (bool)
import Data.Text (Text)
import Reflex.Dom

import Backend.Status
    ( DaoStatus (..)
    , DelegateTxStatus (..)
    , VoteTxStatus (..)
    , WalletStatus (..)
    , isDaoBuffer
    , isDaoTotalBlock
    , isDelegateTxProcess
    , isVoteTxProcess
    , isWalletError
    , messageDaoStatus
    )
import Backend.Wallet
    ( LucidConfig (..)
    , Wallet (..)
    , WalletName (..)
    , fromJS
    , hasToken
    , lucidConfigDao
    )
import Common.Events
import Common.Reflex.Dom.Extra (elementResultJS)
import Common.Reflex.Extra (foldDynamicAny)
import Common.Utility (toText)
import Config.Config (NetworkConfig (dao), networkConfig)
import ENCOINS.Common.Widgets.Advanced (walletError)
import qualified I18n.Reflex.I18n as I18n
import qualified I18n.Common as I18n
import I18n.I18n (App)

handleStatus ::
    (App t m) =>
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

    let currentStatus =
            leftmost
                [ VoteTx <$> eVoteStatus
                , DelegateTx <$> eDelegateStatus
                , WalletInDao <$> updated dUnexpectedNetworkS
                , WalletInDao <$> updated dWalletNotConnectedS
                , WalletInDao <$> eWalletError
                , WalletInDao <$> eHasNotTokenStatus
                ]
    dNotification <- foldDyn processStatus DaoReady currentStatus

    dLocale <- I18n.askLocale
    let localizer ::
            I18n.Locale
            -> Either I18n.StatusMessage (I18n.StatusMessage, I18n.StatusMessage)
            -> Text
        localizer l = \case
            Left m -> I18n.localizeWith l m
            Right (m1, m2) -> I18n.localizeWith l m1 <> I18n.localizeWith l m2
    let dStatusMessage = messageDaoStatus <$> dNotification
    let dStatusText = zipDynWith localizer dLocale dStatusMessage
    logDyn "DaoStatus" $ localizer I18n.Locale_EN <$> dStatusMessage

    pure
        ( dIsDisableButtons
        , dIsDisableConnectButton
        , dStatusText
        )

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
                    then (True, WalletFail "Wallet is not connected!")
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
                (WalletFail "No ENCS tokens to delegate!")
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
    "Unexpected network! Please switch the wallet to mode: "
        <> toText (dao networkConfig)
