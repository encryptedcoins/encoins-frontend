{-# LANGUAGE RecursiveDo #-}

module Backend.EncoinsTx where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified CSL
import           Data.List                 ((\\))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Text                 (Text)
import           JS.App                    (walletSignTx)
import           PlutusTx.Prelude          (length)
import           Prelude                   hiding (length)
import           Reflex.Dom                hiding (Input)
import           Servant.Reflex            (BaseUrl (..))
import           Witherable                (catMaybes)

import           Backend.Protocol.Setup    (encoinsCurrencySymbol,
                                            ledgerAddress, minAdaTxOutInLedger)
import           Backend.Protocol.Types
import           Backend.Protocol.Utility  (getEncoinsInUtxos, mkRedeemer)
import           Backend.Servant.Requests
import           Backend.Status            (Status (..))
import           Backend.Utility           (switchHoldDyn, toEither)
import           Backend.Wallet            (Wallet (..), toJS)
import           ENCOINS.App.Widgets.Basic (elementResultJS)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils      (toText)

encoinsTxWalletMode :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t BulletproofParams
  -> Behavior t Randomness
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> Dynamic t [Text]
  -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxWalletMode
  dWallet
  dBulletproofParams
  bRandomness
  dCoinsBurn
  dCoinsMint
  eSend
  dUrls = mdo
    let eFallback = leftmost [() <$ eNewTxRelayDown, () <$ eSubmitRelayDown]
    -- logEvent "encoinsTxWalletMode: eFallback" eFallback
    let eFailedUrl = leftmost [Nothing <$ eSend, tagPromptlyDyn dmUrl eFallback]
    -- logEvent "encoinsTxWalletMode: eFailedUrl" eFailedUrl

    dFailedUrls <- foldDyn (\mUrl acc -> maybe acc (\u -> u : acc) mUrl) [] eFailedUrl
    -- logDyn "encoinsTxWalletMode: dFailedUrls" dFailedUrls
    let dUpdatedUrls = zipDynWith (\\) dUrls dFailedUrls
    -- logDyn "encoinsTxWalletMode: dUpdatedUrls" dUpdatedUrls

    -- eDelayGetUrl <- delay 1 $ leftmost [eSend, eFallback]

    emUrl <- getRelayUrlE dUpdatedUrls $ leftmost [eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl
    -- logDyn "encoinsTxWalletMode: dmUrl" dmUrl

    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet

    -- Obtaining Secrets and [MintingPolarity]
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer WalletMode ledgerAddress
          <$> current dAddrWallet
          <*> current dBulletproofParams
          <*> current dSecrets
          <*> current dMPs
          <*> bRandomness

    eFireRedeemer <- delay 1 $ leftmost [eSend, () <$ eFallback]

    -- Constructing the final redeemer
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eFireRedeemer
    let eFinalRedeemer = void $ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    let dNewTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) WalletMode) dFinalRedeemer)
          dInputs
    eeNewTxResponse <- switchHoldDyn dmUrl $ \case
      Nothing -> pure never
      Just url -> newTxRequestWrapper
        (BasePath url)
        dNewTxReqBody
        eFinalRedeemer
    let (eNewTxRelayDown, eNewTxBackendError, eNewTxSuccess) =
          fromRelayResponse eeNewTxResponse

    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    -- Signing the transaction
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ updated dWalletSignature

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    eeSubmitResponse <- switchHoldDyn dmUrl $ \case
      Nothing  -> pure never
      Just url -> submitTxRequestWrapper (BasePath url) dSubmitReqBody eWalletSignature
    let (eSubmitRelayDown, eSubmitBackendError, eSubmitted) =
          fromRelayResponse eeSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost
          [ Ready        <$ eConfirmed
          , NoRelay      <$ eAllRelayDown
          , Constructing <$ eFinalRedeemer
          , Signing      <$ eNewTxSuccess
          , Submitting   <$ eWalletSignature
          , Submitted    <$ eSubmitted
          , (BackendError . ("Relay disconnected: " <>) . toText) <$>
              leftmost [eNewTxRelayDown, eSubmitRelayDown]
          , (BackendError . ("Relay returned error: " <>) . toText) <$>
            leftmost [eNewTxBackendError, eSubmitBackendError]
          ]
    logEvent "encoinsTxWalletMode: eStatus" eStatus
    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)

encoinsTxTransferMode :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Secrets
  -> Dynamic t [(Secret, Text)]
  -> Dynamic t (Maybe Address)
  -> Event t ()
  -> Dynamic t [(Text, Text)]
  -> Dynamic t [Text]
  -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxTransferMode
  dWallet
  dCoins
  dNames
  dmAddr
  eSend
  dWalletSignature
  dUrls = mdo

    let eFallback = leftmost [() <$ eNewTxRelayDown, () <$ eSubmitRelayDown]
    emUrl <- getRelayUrlE dUrls $ leftmost [eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl

    eFireTx <- delay 1 $ leftmost [eSend, () <$ eFallback]

    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet
        dAddr       = fromMaybe ledgerAddress <$> dmAddr

    -- Create transaction
    let dNewTxReqBody = zipDyn
          (InputSending <$> dAddr <*> zipDynWith mkValue dCoins dNames <*> dAddrWallet)
          dInputs
    eeNewTxResponse <- switchHoldDyn dmUrl $ \case
      Nothing -> pure never
      Just url -> newTxRequestWrapper
        (BasePath url)
        dNewTxReqBody
        eFireTx
    let (eNewTxRelayDown, eNewTxBackendError, eNewTxSuccess) =
          fromRelayResponse eeNewTxResponse

    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    -- Signing the transaction
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ gate ((/="") <$> current dTx) (updated dWalletSignature)

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    eeSubmitResponse <- switchHoldDyn dmUrl $ \case
      Nothing  -> pure never
      Just url -> submitTxRequestWrapper (BasePath url) dSubmitReqBody eWalletSignature
    let (eSubmitRelayDown, eSubmitBackendError, eSubmitted) =
          fromRelayResponse eeSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost
          [ Ready        <$ eConfirmed
          , NoRelay      <$ eAllRelayDown
          , Constructing <$ eFireTx
          , Signing      <$ eNewTxSuccess
          , Submitting   <$ eWalletSignature
          , Submitted    <$ eSubmitted
          , (BackendError . ("Relay disconnected: " <>) . toText) <$>
              leftmost [eNewTxRelayDown, eSubmitRelayDown]
          , (BackendError . ("Relay returned error: " <>) . toText) <$>
             leftmost [eNewTxBackendError, eSubmitBackendError]
          ]
    logEvent "encoinsTxTransferMode: eStatus" eStatus
    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)
  where
    mkValue coins names = CSL.Value (toText $ minAdaTxOutInLedger * length coins)
      . Just
      . CSL.MultiAsset
      . Map.singleton encoinsCurrencySymbol
      . Map.fromList
      . mapMaybe (\s -> (,"1") <$> lookup s names)
      $ coins

encoinsTxLedgerMode :: MonadWidget t m
  => Dynamic t BulletproofParams
  -> Behavior t Randomness
  -> Dynamic t Address
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> Dynamic t [Text]
  -> m (Dynamic t [Text], Event t Status)
encoinsTxLedgerMode
  dBulletproofParams
  bRandomness
  dChangeAddr
  dCoinsBurn
  dCoinsMint
  eSend
  dUrls = mdo
    eInit <- delay 1 =<< newEvent

    let eFallback = leftmost [() <$ eStatusRelayDown, () <$ eServerRelayDown]
    emUrl <- getRelayUrlE dUrls $ leftmost [eInit, eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl

    eTick <- tickLossyFromPostBuildTime 12
    eFireStatus <- delay 1 $ leftmost [eInit, void eTick, eFallback]

    eeStatus <- switchHoldDyn dmUrl $ \case
      Nothing  -> pure never
      Just url -> statusRequestWrapper
        (BasePath url)
        (pure LedgerEncoins)
        eFireStatus
    let (eStatusRelayDown, eStatusBackendError, eStatusResp) =
          fromRelayResponse eeStatus

    let toLedgerUtxoResult (LedgerUtxoResult xs) = Just xs
        toLedgerUtxoResult _                     = Nothing
        eLedgerUtxoResult = mapMaybe toLedgerUtxoResult eStatusResp
    dUTXOs <- holdDyn [] eLedgerUtxoResult
    let dInputs = map CSL.input <$> dUTXOs

    -- Obtaining Secrets and [MintingPolarity]
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer LedgerMode ledgerAddress
          <$> current dChangeAddr
          <*> current dBulletproofParams
          <*> current dSecrets
          <*> current dMPs
          <*> bRandomness

    -- Constructing the final redeemer
    eFireTx <- delay 0.1 $ leftmost [eSend, eFallback] -- NOTE: The delay is needed here to wait for change address to be updated
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eFireTx
    let eFinalRedeemer = void $ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    let dServerTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) LedgerMode) dFinalRedeemer) dInputs
    eFinalRedeemerReq <- delay 1 $ void $ tagPromptlyDyn dServerTxReqBody eFinalRedeemer
    logEvent "Fire ledger tx" eFinalRedeemerReq

    eeServerTxResponse <- switchHoldDyn dmUrl $ \case
      Just url -> serverTxRequestWrapper
        (BasePath url)
        dServerTxReqBody
        eFinalRedeemerReq
      Nothing  -> pure never
    let (eServerRelayDown, eServerBackendError, eServerOk) =
          fromRelayResponse eeServerTxResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost
          [ Ready        <$ eConfirmed
          , NoRelay      <$ eAllRelayDown
          , Constructing <$ eFinalRedeemer
          , Submitted    <$ eServerOk
          , (BackendError . ("Relay disconnected: " <>) . toText) <$>
              leftmost [eStatusRelayDown, eServerRelayDown]
          , (BackendError . ("Relay returned error: " <>) . toText) <$>
              leftmost [eStatusBackendError, eServerBackendError]
          ]
    logEvent "encoinsTxLedgerMode: eStatus" eStatus
    return (fmap getEncoinsInUtxos dUTXOs, eStatus)
