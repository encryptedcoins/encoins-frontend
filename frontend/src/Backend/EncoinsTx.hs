{-# LANGUAGE RecursiveDo #-}

module Backend.EncoinsTx where

import           Control.Monad             (void, (<=<))
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified CSL
import           Data.Functor              ((<&>))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, fromMaybe, isJust,
                                            isNothing)
import           Data.Text                 (Text)
import           Data.Time
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
import           Backend.Status            (Status (..), everyRelayDown,
                                            relayError)
import           Backend.Wallet            (Wallet (..), toJS)
import           ENCOINS.App.Widgets.Basic (elementResultJS, relayStatusM,
                                            tellRelayStatus)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Common.Events     (logDyn, logEvent, newEvent)
import           ENCOINS.Common.Utils      (toText)

import           Debug.Trace

encoinsTxWalletMode :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Dynamic t Wallet
  -> Dynamic t BulletproofParams
  -> Behavior t Randomness
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxWalletMode
  dWallet
  dBulletproofParams
  bRandomness
  dCoinsBurn
  dCoinsMint
  eSend = mdo

    -- now <- liftIO getCurrentTime
    -- evTick <- (() <$) <$> tickLossy 5 now

    -- emUrlTest <- getRelayUrlE evTick
    -- logEvent "encoinsTxWalletMode: emUrlTest:" emUrlTest

    -- traceM $ "urls" <> show urls
    -- shuffled <- shuffle urls $ () <$ evTick
    -- logEvent "shuffled" shuffled
    -- let emUrl = (Just $ BasePath "http://localhost:3000") <$ ev
    -- emUrl <- getValidRelay $ () <$ updated evTick

    ev <- newEvent
    emUrl <- getRelayUrlE $ leftmost [eSend, ev]
    logEvent "encoinsTxWalletMode: emUrl:" emUrl
    -- tellRelayStatus emUrl

    -- emUrlNewTx <- getRelayUrlE $ () <$ eNewTxRelayDown
    -- tellRelayStatus emUrlNewTx

    dmUrl <- holdDyn Nothing emUrl
    logDyn "encoinsTxWalletMode: dmUrl: " dmUrl
    logEvent "encoinsTxWalletMode: updated dmUrl: " $ updated dmUrl


    -- mBaseUrl <- getRelayUrl -- this chooses random server with successful ping
    -- relayStatusM mBaseUrl

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

    -- Constructing the final redeemer
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eSend
    let eFinalRedeemer = void $ catMaybes (updated dFinalRedeemer)

    logEvent "eFinalRedeemer" eFinalRedeemer
    -- Constructing a new transaction
    let dNewTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) WalletMode) dFinalRedeemer)
          dInputs
    logDyn "dNewTxReqBody" dNewTxReqBody
    -- (eNewTxSuccess, eRelayDown) <- case mBaseUrl of
    --   Just baseUrl -> newTxRequestWrapper baseUrl dNewTxReqBody eFinalRedeemer
    --   _            -> pure (never, never)

    eeNewTxResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing -> pure never
      Just url -> newTxRequestWrapper
        url
        dNewTxReqBody
        eFinalRedeemer
    let (eNewTxSuccess, eNewTxRelayDown) =
          eventMaybe (BackendError relayError) eeNewTxResponse

    logEvent "eeNewTxResponse" eeNewTxResponse
    logEvent "eNewTxRelayDown" eNewTxRelayDown
    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    logEvent "eTxId" eTxId
    logEvent "eTx" eTx

    -- Signing the transaction
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ updated dWalletSignature

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature

    -- (eSubmitted, eRelayDown') <- case mBaseUrl of
    --   Just baseUrl -> submitTxRequestWrapper baseUrl dSubmitReqBody eWalletSignature
    --   _            -> pure (never, never)

    eeSubmitResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing -> pure never
      Just url -> submitTxRequestWrapper url dSubmitReqBody eWalletSignature
    let (eSubmitted, eSubmitRelayDown) =
          eventMaybe (BackendError relayError) eeSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          Constructing <$ eFinalRedeemer,
          -- eRelayDown,
          eNewTxRelayDown,
          Signing      <$ eNewTxSuccess,
          Submitting   <$ eWalletSignature,
          -- eRelayDown',
          eSubmitRelayDown,
          Submitted    <$ eSubmitted
          ]
    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)

encoinsTxTransferMode :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Dynamic t Wallet
  -> Dynamic t Secrets
  -> Dynamic t [(Secret, Text)]
  -> Dynamic t (Maybe Address)
  -> Event t ()
  -> Dynamic t [(Text, Text)]
  -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxTransferMode
  dWallet
  dCoins
  dNames
  dmAddr
  eSend
  dWalletSignature = mdo

    -- emUrl <- getValidRelay $ leftmost [ev, () <$ eNewTxRelayDown, () <$ eSubmitRelayDown]
    -- logEvent "encoinsTxWalletMode: emUrl: " emUrl
    -- let emUrl = (Just $ BasePath "http://localhost:3000") <$ ev
    -- emUrl <- getRelayUrlE ev
    -- logEvent "encoinsTxWalletMode: getRelayUrlE: emUrl:" emUrl
    -- dUrl <- foldDynMaybe const (BasePath "") emUrl
    -- logDyn "encoinsTxWalletMode: dUrl: " dUrl

    -- tellRelayStatus
    --   "Relay status"
    --   (BackendError relayError)
    --   (fmapMaybe (\mU -> if isNothing mU then Just () else Nothing) emUrl)
    ev <- newEvent
    emUrl <- getRelayUrlE ev
    logEvent "encoinsTxWalletMode: emUrl:" emUrl
    tellRelayStatus emUrl

    dUrl <- foldDynMaybe const (BasePath "") emUrl
    logDyn "encoinsTxWalletMode: dUrl: " dUrl
    -- mBaseUrl <- getRelayUrl -- this chooses random server with successful ping
    -- relayStatusM mBaseUrl

    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet
        dAddr       = fromMaybe ledgerAddress <$> dmAddr

    -- Constructing a new transaction
    -- (eNewTxSuccess, eRelayDown) <- case mBaseUrl of
    --   Just baseUrl ->
    --     newTxRequestWrapper
    --       baseUrl
    --       (zipDyn (InputSending <$> dAddr <*> zipDynWith mkValue dCoins dNames <*> dAddrWallet) dInputs)
    --       eSend
    --   _            -> pure (never, never)

    eeNewTxResponse <- switchHold never <=< dyn $ dUrl <&> \url ->
        newTxRequestWrapper
          url
          (zipDyn (InputSending <$> dAddr <*> zipDynWith mkValue dCoins dNames <*> dAddrWallet) dInputs)
          eSend
    let (eNewTxSuccess, eNewTxRelayDown) =
            eventMaybe (BackendError relayError) eeNewTxResponse

    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    -- Signing the transaction
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ gate ((/="") <$> current dTx) (updated dWalletSignature)

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    -- (eSubmitted, eRelayDown') <- case mBaseUrl of
    --   Just baseUrl -> submitTxRequestWrapper baseUrl dSubmitReqBody eWalletSignature
    --   _            -> pure (never, never)

    eeSubmitResponse <- switchHold never <=< dyn $ dUrl <&> \url ->
      submitTxRequestWrapper url dSubmitReqBody eWalletSignature
    logEvent "submitTxRequestWrapper: url:" $ tagPromptlyDyn dUrl eeSubmitResponse
    let (eSubmitted, eSubmitRelayDown) =
          eventMaybe (BackendError relayError) eeSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          -- eRelayDown,
          eNewTxRelayDown,
          Signing      <$ eNewTxSuccess,
          Constructing <$ eSend,
          Submitting   <$ eWalletSignature,
          -- eRelayDown',
          eSubmitRelayDown,
          Submitted    <$ eSubmitted
          ]
    logEvent "encoinsTxTransferMode: eStatus" eStatus

    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)
  where
    mkValue coins names = CSL.Value (toText $ minAdaTxOutInLedger * length coins) . Just . CSL.MultiAsset . Map.singleton
      encoinsCurrencySymbol . Map.fromList . mapMaybe
        (\s -> (,"1") <$> lookup s names) $ coins

encoinsTxLedgerMode :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Dynamic t Wallet
  -> Dynamic t BulletproofParams
  -> Behavior t Randomness
  -> Dynamic t (Maybe Address)
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> m (Dynamic t [Text], Event t Status)
encoinsTxLedgerMode
  dWallet
  dBulletproofParams
  bRandomness
  dmChangeAddr
  dCoinsBurn
  dCoinsMint
  eSend = mdo
    mBaseUrl <- getRelayUrl -- this chooses random server with successful ping
    relayStatusM mBaseUrl

    ePb   <- getPostBuild
    eTick <- tickLossyFromPostBuildTime 12
    emStatus <- case mBaseUrl of
      Just baseUrl ->
          statusRequestWrapper
            baseUrl
            (pure LedgerEncoins)
            (leftmost [ePb, void eTick])
      Nothing      -> pure never
    let (eStatusResp, eRelayDown') = eventMaybe (BackendError relayError) emStatus

    let toLedgerUtxoResult (LedgerUtxoResult xs) = Just xs
        toLedgerUtxoResult _                     = Nothing
        eLedgerUtxoResult = mapMaybe toLedgerUtxoResult eStatusResp
    dUTXOs <- holdDyn [] eLedgerUtxoResult
    let dInputs = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        dChangeAddr = zipDynWith fromMaybe dAddrWallet dmChangeAddr

    -- Obtaining Secrets and [MintingPolarity]
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer LedgerMode ledgerAddress <$> current dChangeAddr <*>
          current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- Constructing the final redeemer
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eSend
    let eFinalRedeemer = void $ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    let dServerTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) LedgerMode) dFinalRedeemer) dInputs
    (eServerOk, eRelayDown) <- case mBaseUrl of
      Just baseUrl -> serverTxRequestWrapper baseUrl dServerTxReqBody eFinalRedeemer
      _            -> pure (never, never)

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs


    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          Constructing <$ eFinalRedeemer,
          eRelayDown,
          Submitted    <$ eServerOk,
          eRelayDown' ]
    return (fmap getEncoinsInUtxos dUTXOs, eStatus)
