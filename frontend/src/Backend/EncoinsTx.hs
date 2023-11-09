{-# LANGUAGE RecursiveDo #-}

module Backend.EncoinsTx where

import           Control.Monad             (void, (<=<))
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified CSL
import           Data.Functor              ((<&>))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Text                 (Text)
import           JS.App                    (walletSignTx)
import           PlutusTx.Prelude          (length)
import           Prelude                   hiding (length)
import           Reflex.Dom                hiding (Input)
import           Witherable                (catMaybes)

import           Backend.Protocol.Setup    (encoinsCurrencySymbol,
                                            ledgerAddress, minAdaTxOutInLedger)
import           Backend.Protocol.Types
import           Backend.Protocol.Utility  (getEncoinsInUtxos, mkRedeemer)
import           Backend.Servant.Requests
import           Backend.Status            (Status (..), relayError)
import           Backend.Utility           (toEither)
import           Backend.Wallet            (Wallet (..), toJS)
import           ENCOINS.App.Widgets.Basic (elementResultJS)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils      (toText)


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

    let eFallback = leftmost [() <$ eNewTxRelayDown, () <$ eSubmitRelayDown]
    emUrl <- getRelayUrlE $ leftmost [eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl

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

    emNewTxResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing -> pure never
      Just url -> newTxRequestWrapper
        url
        dNewTxReqBody
        eFinalRedeemer
    let (eNewTxSuccess, eNewTxRelayDown) =
          eventMaybe (BackendError relayError) emNewTxResponse

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

    emSubmitResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing  -> pure never
      Just url -> submitTxRequestWrapper url dSubmitReqBody eWalletSignature
    let (eSubmitted, eSubmitRelayDown) =
          eventMaybe (BackendError relayError) emSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          NoRelay      <$ eAllRelayDown,
          Constructing <$ eFinalRedeemer,
          eNewTxRelayDown,
          Signing      <$ eNewTxSuccess,
          Submitting   <$ eWalletSignature,
          eSubmitRelayDown,
          Submitted    <$ eSubmitted
          ]
    logEvent "encoinsTxWalletMode: eStatus" eStatus
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

    let eFallback = leftmost [() <$ eNewTxRelayDown, () <$ eSubmitRelayDown]
    emUrl <- getRelayUrlE $ leftmost [eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl

    eFireTx <- delay 1 $ leftmost [eSend, () <$ eFallback]

    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs

    let dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet
        dAddr       = fromMaybe ledgerAddress <$> dmAddr

    -- Create transaction
    emNewTxResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing -> pure never
      Just url -> newTxRequestWrapper
        url
        (zipDyn (InputSending <$> dAddr <*> zipDynWith mkValue dCoins dNames <*> dAddrWallet) dInputs)
        eFireTx
    let (eNewTxSuccess, eNewTxRelayDown) =
            eventMaybe (BackendError relayError) emNewTxResponse

    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx   <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    -- Signing the transaction
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = void $ gate ((/="") <$> current dTx) (updated dWalletSignature)

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature

    emSubmitResponse <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing  -> pure never
      Just url -> submitTxRequestWrapper url dSubmitReqBody eWalletSignature
    let (eSubmitted, eSubmitRelayDown) =
          eventMaybe (BackendError relayError) emSubmitResponse

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost [
          Ready        <$ eConfirmed,
          NoRelay      <$ eAllRelayDown,
          Constructing <$ eFireTx,
          eNewTxRelayDown,
          Signing      <$ eNewTxSuccess,
          Submitting   <$ eWalletSignature,
          eSubmitRelayDown,
          Submitted    <$ eSubmitted
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
    ePb   <- getPostBuild
    eTick <- tickLossyFromPostBuildTime 12

    let eFallback = leftmost [() <$ eStatusRelayDown, () <$ eServerRelayDown]
    emUrl <- getRelayUrlE $ leftmost [ePb, eSend, eFallback]
    dmUrl <- holdDyn Nothing emUrl

    eFireStatus <- delay 1 $ leftmost [ePb, void eTick, eFallback]

    emStatus <- switchHold never <=< dyn $ dmUrl <&> \case
      Nothing  -> pure never
      Just url -> statusRequestWrapper url (pure LedgerEncoins) eFireStatus
    let (eStatusResp, eStatusRelayDown) = eventMaybe (BackendError relayError) emStatus

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
    let bRed = mkRedeemer LedgerMode ledgerAddress
          <$> current dChangeAddr
          <*> current dBulletproofParams
          <*> current dSecrets
          <*> current dMPs
          <*> bRandomness

    -- Constructing the final redeemer
    let eFireTx = leftmost [eSend, () <$ eServerRelayDown]
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eFireTx
    let eFinalRedeemer = void $ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    let dServerTxReqBody = zipDyn
          (fmap (\r -> InputRedeemer (fromJust r) LedgerMode) dFinalRedeemer) dInputs
    eFinalRedeemerReq <- delay 1 $ void $ tagPromptlyDyn dServerTxReqBody eFinalRedeemer
    eeServerTx <- switchHold never <=< dyn $ dmUrl <&> \case
      Just url -> serverTxRequestWrapper url dServerTxReqBody eFinalRedeemerReq
      Nothing  -> pure never
    let (eServerStatus, eServerOk) = eventEither eeServerTx
    let eeServerStatus = hasStatusZero <$> eServerStatus
    let (eServerRelayDown, eBackendErrorResp) =
          (filterLeft eeServerStatus, filterRight eeServerStatus)

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl

    let eStatus = leftmost
          [ Ready        <$ eConfirmed
          , NoRelay      <$ eAllRelayDown
          , eStatusRelayDown
          , Constructing <$ eFinalRedeemer
          , Submitted    <$ eServerOk
          , (BackendError . toText) <$> eBackendErrorResp
          , (BackendError . toText) <$> eServerRelayDown
          ]
    logEvent "encoinsTxLedgerMode: eStatus" eStatus
    return (fmap getEncoinsInUtxos dUTXOs, eStatus)
