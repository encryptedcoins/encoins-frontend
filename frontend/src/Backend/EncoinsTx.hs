{-# LANGUAGE RecursiveDo #-}

module Backend.EncoinsTx where

import qualified CSL
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import JS.App (walletSignTx)
import PlutusTx.Prelude (length)
import Reflex.Dom hiding (Input)
import Servant.Reflex (BaseUrl (..))
import Witherable (catMaybes)
import Prelude hiding (length)

import Backend.Protocol.Setup
    ( encoinsCurrencySymbol
    , ledgerAddress
    , minAdaTxOutInLedger
    )
import Backend.Protocol.Types
import Backend.Protocol.Utility
    ( getEncoinsInUtxos
    , mkLedgerRedeemer
    , mkWalletRedeemer
    )
import Backend.Servant.Requests
import Backend.Status (Status (..))
import Backend.Utility
    ( eventEither
    , eventMaybe
    , switchHoldDyn
    , toEither
    , toText
    )
import Backend.Wallet (Wallet (..), toJS)
import ENCOINS.App.Widgets.Basic (elementResultJS)
import ENCOINS.BaseTypes
import ENCOINS.Bulletproofs
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Advanced (fireWhenJustThenReset, updateUrls)

encoinsTxWalletMode ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t BulletproofParams
    -> Behavior t Randomness
    -> Dynamic t Secrets
    -> Dynamic t Secrets
    -> Event t ()
    -> Dynamic t [Text]
    -> m (Dynamic t [AssetName], Event t Status, Dynamic t Text)
encoinsTxWalletMode
    dWallet
    dBulletproofParams
    bRandomness
    dCoinsBurn
    dCoinsMint
    eSend
    dUrls = mdo
        let eFallback = leftmost [() <$ eNewTxError, () <$ eSubmitError]
        let emUsedUrl = leftmost [Nothing <$ eSend, tagPromptlyDyn dmUrl eFallback]
        dUpdatedUrls <- updateUrls dUrls emUsedUrl
        emUsedUrlDelayed <- delay 0.01 $ () <$ emUsedUrl
        emUrl <- getRelayUrlE dUpdatedUrls emUsedUrlDelayed
        dmUrl <- holdDyn Nothing emUrl

        let dUTXOs = fmap walletUTXOs dWallet
            dInputs = map CSL.input <$> dUTXOs

        let dAddrWallet = fmap walletChangeAddress dWallet
            bWalletName = toJS . walletName <$> current dWallet

        -- Obtaining Secrets and [MintingPolarity]
        let dLst =
                unzip
                    <$> zipDynWith
                        (++)
                        (fmap (map (,Burn)) dCoinsBurn)
                        (fmap (map (,Mint)) dCoinsMint)
            dSecrets = fmap fst dLst
            dMPs = fmap snd dLst

        -- Obtaining EncoinsRedeemer
        let bRed =
                mkWalletRedeemer WalletMode ledgerAddress
                    <$> current dAddrWallet
                    <*> current dBulletproofParams
                    <*> current dSecrets
                    <*> current dMPs
                    <*> bRandomness

        eSendWithUrl <- fireWhenJustThenReset eSend emUrl eFinalRedeemer
        eFallbackWithUrl <- fireWhenJustThenReset eFallback emUrl eFinalRedeemer

        eFireRedeemer <- delay 1 $ leftmost [eSendWithUrl, eFallbackWithUrl]

        -- Constructing the final redeemer
        dmFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eFireRedeemer
        let eFinalRedeemer = void $ catMaybes (updated dmFinalRedeemer)

        -- Constructing a new transaction
        let dNewTxReqBody =
                zipDyn
                    (fmap (\r -> InputRedeemer (fromJust r) WalletMode) dmFinalRedeemer)
                    dInputs
        eeNewTxResponse <- switchHoldDyn dmUrl $ \case
            Nothing -> pure never
            Just url ->
                newTxRequestWrapper
                    (BasePath url)
                    dNewTxReqBody
                    eFinalRedeemer
        let (eNewTxError, eNewTxSuccess) = eventEither eeNewTxResponse

        let eTxId = fmap fst eNewTxSuccess
            eTx = fmap snd eNewTxSuccess
        dTx <- holdDyn "" eTx
        dTxId <- holdDyn "" eTxId

        -- Signing the transaction
        dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
        performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
        let eWalletSignature = void $ updated dWalletSignature

        -- Submitting the transaction
        let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
        eeSubmitResponse <- switchHoldDyn dmUrl $ \case
            Nothing -> pure never
            Just url ->
                submitTxRequestWrapper
                    (BasePath url)
                    dSubmitReqBody
                    eWalletSignature
        let (eSubmitError, eSubmitted) = eventEither eeSubmitResponse

        -- Tracking the pending transaction
        eConfirmed <- updated <$> holdUniqDyn dUTXOs
        let eAllRelayDown = filterLeft $ toEither () <$> emUrl

        let eStatus =
                leftmost
                    [ Ready <$ eConfirmed
                    , NoRelay <$ eAllRelayDown
                    , Constructing <$ eFireRedeemer
                    , Signing <$ eNewTxSuccess
                    , Submitting <$ eWalletSignature
                    , Submitted <$ eSubmitted
                    , (BackendError . ("Relay returned error: " <>))
                        <$> leftmost [eNewTxError, eSubmitError]
                    ]
        return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)

encoinsTxTransferMode ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t Secrets
    -> Dynamic t [TokenCacheV3]
    -> Dynamic t (Maybe Address)
    -> Event t ()
    -> Dynamic t [(Text, Text)]
    -> Dynamic t [Text]
    -> m (Dynamic t [AssetName], Event t Status, Dynamic t Text)
encoinsTxTransferMode
    dWallet
    dCoins
    dTokenCache
    dmAddr
    eSend
    dWalletSignature
    dUrls = mdo
        let eFallback = leftmost [() <$ eNewTxError, () <$ eSubmitError]
        let emUsedUrl = leftmost [Nothing <$ eSend, tagPromptlyDyn dmUrl eFallback]
        dUpdatedUrls <- updateUrls dUrls emUsedUrl
        emUsedUrlDelayed <- delay 0.01 $ () <$ emUsedUrl
        emUrl <- getRelayUrlE dUpdatedUrls emUsedUrlDelayed
        dmUrl <- holdDyn Nothing emUrl

        eSendWithUrl <- fireWhenJustThenReset eSend emUrl eFireTx
        eFallbackWithUrl <- fireWhenJustThenReset eFallback emUrl eFireTx

        let dUTXOs = fmap walletUTXOs dWallet
            dInputs = map CSL.input <$> dUTXOs

        let dAddrWallet = fmap walletChangeAddress dWallet
            bWalletName = toJS . walletName <$> current dWallet
            dAddr = fromMaybe ledgerAddress <$> dmAddr

        -- Create transaction
        let dNewTxReqBody =
                zipDyn
                    ( InputSending
                        <$> dAddr
                        <*> zipDynWith mkValue dCoins dTokenCache
                        <*> dAddrWallet
                    )
                    dInputs

        eFireTx <-
            delay 1 $
                leftmost
                    [ eSendWithUrl
                    , eFallbackWithUrl
                    ]

        eeNewTxResponse <- switchHoldDyn dmUrl $ \case
            Nothing -> pure never
            Just url ->
                newTxRequestWrapper
                    (BasePath url)
                    dNewTxReqBody
                    eFireTx
        let (eNewTxError, eNewTxSuccess) = eventEither eeNewTxResponse

        let eTxId = fmap fst eNewTxSuccess
            eTx = fmap snd eNewTxSuccess
        dTx <- holdDyn "" eTx
        dTxId <- holdDyn "" eTxId

        -- Signing the transaction
        performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
        let eWalletSignature = void $ gate ((/= "") <$> current dTx) (updated dWalletSignature)

        -- Submitting the transaction
        let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
        eeSubmitResponse <- switchHoldDyn dmUrl $ \case
            Nothing -> pure never
            Just url -> submitTxRequestWrapper (BasePath url) dSubmitReqBody eWalletSignature
        let (eSubmitError, eSubmitted) = eventEither eeSubmitResponse

        -- Tracking the pending transaction
        eConfirmed <- updated <$> holdUniqDyn dUTXOs
        let eAllRelayDown = filterLeft $ toEither () <$> emUrl

        let eStatus =
                leftmost
                    [ Ready <$ eConfirmed
                    , NoRelay <$ eAllRelayDown
                    , Constructing <$ eFireTx
                    , Signing <$ eNewTxSuccess
                    , Submitting <$ eWalletSignature
                    , Submitted <$ eSubmitted
                    , (BackendError . ("Relay returned error: " <>))
                        <$> leftmost [eNewTxError, eSubmitError]
                    ]
        return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)
        where
            mkValue :: Secrets -> [TokenCacheV3] -> CSL.Value
            mkValue coins tokenCaches =
                CSL.Value (toText $ minAdaTxOutInLedger * length coins)
                    . Just
                    . CSL.MultiAsset
                    . Map.singleton encoinsCurrencySymbol
                    . Map.fromList
                    . mapMaybe
                        ( \s ->
                            (,"1") . getAssetName . tcAssetName
                                <$> find (\tc -> s == tcSecret tc) tokenCaches
                        )
                    $ coins

encoinsTxLedgerMode ::
    (MonadWidget t m) =>
    Dynamic t BulletproofParams
    -> Behavior t Randomness
    -> Dynamic t Address
    -> Dynamic t Secrets
    -> Dynamic t Secrets
    -> Event t ()
    -> Dynamic t [Text]
    -> m (Dynamic t [AssetName], Event t Status)
encoinsTxLedgerMode
    dBulletproofParams
    bRandomness
    dChangeAddr
    dCoinsBurn
    dCoinsMint
    eSend
    dUrls = mdo
        eInit <- delay 0.01 =<< newEvent

        let emUsedUrl =
                leftmost
                    [ Nothing <$ eInit
                    , Nothing <$ eSend
                    , tagPromptlyDyn dmUrl eStatusError
                    , tagPromptlyDyn dmUrl eServerError
                    ]

        dUpdatedUrls <- updateUrls dUrls emUsedUrl
        -- Wait 0.01s until urls are updated
        emUsedUrlDelayed <- delay 0.01 $ () <$ emUsedUrl
        emUrl <- getRelayUrlE dUpdatedUrls emUsedUrlDelayed
        dmUrl <- holdDyn Nothing emUrl

        -- Initially run status just one time with valid url without waiting 12 secs.
        eInitWithUrl <- fireWhenJustThenReset eInit emUrl eFireStatus

        -- Fire Status Fallback only when there is valid url.
        eStatusFallbackWithUrl <- fireWhenJustThenReset eStatusError emUrl eFireStatus

        -- Fire Server Fallback only when there is valid url.
        eServerFallbackWithUrl <-
            fireWhenJustThenReset eServerError emUrl eFinalRedeemerReq

        -- Fire Send event only when there is valid url.
        eSendWithUrl <- fireWhenJustThenReset eSend emUrl eFinalRedeemerReq

        eTick <- tickLossyFromPostBuildTime 12
        -- Wait 0.01s until pinging url is chosen
        eFireStatus <-
            delay 0.01 $ leftmost [eInitWithUrl, void eTick, eStatusFallbackWithUrl]

        eeStatus <- switchHoldDyn dmUrl $ \case
            Nothing -> pure never
            Just url ->
                statusRequestWrapper
                    (BasePath url)
                    (pure LedgerEncoins)
                    eFireStatus
        let (eStatusError, eStatusResp) = eventEither eeStatus

        let toLedgerUtxoResult (LedgerUtxoResult xs) = Just xs
            toLedgerUtxoResult _ = Nothing
            eLedgerUtxoResult = mapMaybe toLedgerUtxoResult eStatusResp
        dUTXOs <- holdDyn [] eLedgerUtxoResult
        let dInputs = map CSL.input <$> dUTXOs

        -- Obtaining Secrets and [MintingPolarity]
        let dLst =
                unzip
                    <$> zipDynWith
                        (++)
                        (fmap (map (,Burn)) dCoinsBurn)
                        (fmap (map (,Mint)) dCoinsMint)
            dSecrets = fmap fst dLst
            dMPs = fmap snd dLst

        -- Obtaining EncoinsRedeemer
        let bmRed =
                mkLedgerRedeemer LedgerMode ledgerAddress
                    <$> current dBulletproofParams
                    <*> current dSecrets
                    <*> current dMPs
                    <*> bRandomness
                    <*> current dChangeAddr

        -- Constructing the final redeemer
        -- NOTE: The delay is needed here to wait for change address to be updated
        -- Wait 2 secs until ChangeAddress is updated and Redeemer is made.
        -- We suppose that 2 secs is sufficient.
        eFireTx <- delay 2 $ leftmost [eSendWithUrl, eServerFallbackWithUrl]
        dmFinalRedeemer <- holdDyn Nothing $ bmRed `tag` eFireTx
        let (eInvalidChangeAddress, eFinalRedeemer) =
                eventMaybe InvalidChangeAddress (updated dmFinalRedeemer)

        -- Constructing a new transaction
        let dServerTxReqBody =
                zipDyn
                    (fmap (\r -> InputRedeemer (fromJust r) LedgerMode) dmFinalRedeemer)
                    dInputs
        eFinalRedeemerReq <-
            delay 1 $ void $ tagPromptlyDyn dServerTxReqBody eFinalRedeemer
        logEvent "Fire ledger tx" eFinalRedeemerReq

        eeServerTxResponse <- switchHoldDyn dmUrl $ \case
            Just url ->
                serverTxRequestWrapper
                    (BasePath url)
                    dServerTxReqBody
                    eFinalRedeemerReq
            Nothing -> pure never
        let (eServerError, eServerOk) = eventEither eeServerTxResponse

        -- Tracking the pending transaction
        eConfirmed <- updated <$> holdUniqDyn dUTXOs
        let eAllRelayDown = filterLeft $ toEither () <$> emUrl

        let eStatus =
                leftmost
                    [ Ready <$ eConfirmed
                    , NoRelay <$ eAllRelayDown
                    , eInvalidChangeAddress
                    , Constructing <$ eFireTx
                    , Submitted <$ eServerOk
                    , (BackendError . ("Relay returned error: " <>))
                        <$> leftmost [eStatusError, eServerError]
                    ]
        return (fmap getEncoinsInUtxos dUTXOs, eStatus)
