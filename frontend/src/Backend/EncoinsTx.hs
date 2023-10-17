module Backend.EncoinsTx where

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO(..))
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Text                       (Text)
import           PlutusTx.Prelude                (length)
import           Prelude                         hiding (length)
import           Reflex.Dom                      hiding (Input)
import           Witherable                      (catMaybes)

import           Backend.Protocol.Setup          (encoinsCurrencySymbol, ledgerAddress, minAdaTxOutInLedger)
import           Backend.Protocol.Utility        (getEncoinsInUtxos, mkRedeemer)
import           Backend.Servant.Requests
import           Backend.Status                  (Status (..))
import           Backend.Protocol.Types
import           Backend.Wallet                  (Wallet(..), toJS)
import qualified CSL
import           ENCOINS.App.Widgets.Basic       (elementResultJS, relayStatusM)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Common.Utils            (toText)
import           JS.App                          (walletSignTx)
import           ENCOINS.Common.Events           (logEvent)

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
    mBaseUrl <- getRelayUrl -- this chooses random server with successful ping
    relayStatusM mBaseUrl

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


    -- Constructing a new transaction
    let dNewTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) WalletMode) dFinalRedeemer)
          dInputs
    (eNewTxSuccess, eRelayDown) <- case mBaseUrl of
      Just baseUrl -> do
        void $ versionRequestWrapper (constDyn baseUrl) eFinalRedeemer
        newTxRequestWrapper (constDyn baseUrl) dNewTxReqBody eFinalRedeemer
      _            -> pure (never, never)
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
    (eSubmitted, eRelayDown') <- case mBaseUrl of
      Just baseUrl -> submitTxRequestWrapper (constDyn baseUrl) dSubmitReqBody eWalletSignature
      _            -> pure (never, never)

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          Constructing <$ eFinalRedeemer,
          eRelayDown,
          Signing      <$ eNewTxSuccess,
          Submitting   <$ eWalletSignature,
          eRelayDown',
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
  dWalletSignature = do
    mBaseUrl <- getRelayUrl -- this chooses random server with successful ping
    relayStatusM mBaseUrl

    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet
        dAddr       = fromMaybe ledgerAddress <$> dmAddr

    -- Constructing a new transaction
    (eNewTxSuccess, eRelayDown) <- case mBaseUrl of
      Just baseUrl ->
        newTxRequestWrapper
          (constDyn baseUrl)
          (zipDyn (InputSending <$> dAddr <*> zipDynWith mkValue dCoins dNames <*> dAddrWallet) dInputs)
          eSend
      _            -> pure (never, never)
    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    -- Signing the transaction
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ gate ((/="") <$> current dTx) (updated dWalletSignature)

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    (eSubmitted, eRelayDown') <- case mBaseUrl of
      Just baseUrl -> submitTxRequestWrapper (constDyn baseUrl) dSubmitReqBody eWalletSignature
      _            -> pure (never, never)

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          eRelayDown,
          Signing      <$ eNewTxSuccess,
          Constructing <$ eSend,
          Submitting   <$ eWalletSignature,
          eRelayDown',
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
    (eStatusResp, eRelayDown') <- case mBaseUrl of
      Just baseUrl ->
          statusRequestWrapper
            (constDyn baseUrl)
            (pure LedgerEncoins)
            $ leftmost [ePb, void eTick]
      Nothing      -> pure (never, never)
    let toLedgerUtxoResult (LedgerUtxoResult xs) = Just xs
        toLedgerUtxoResult _ = Nothing
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
      Just baseUrl -> serverTxRequestWrapper (constDyn baseUrl) dServerTxReqBody eFinalRedeemer
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
