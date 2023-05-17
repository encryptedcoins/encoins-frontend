module Backend.EncoinsTx where

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.Aeson                      (decode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.FileEmbed                  (embedFile)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Text                       (Text, pack)
import qualified Data.Text                       as Text
import           PlutusTx.Builtins
import           Reflex.Dom                      hiding (Input)
import           System.Random.Stateful          (randomIO)
import           Text.Hex                        (encodeHex, decodeHex)
import           Witherable                      (catMaybes)

import           Backend.Servant.Client          (pabIP)
import           Backend.Servant.Requests
import           Backend.Status                  (Status (..))
import           Backend.Types
import           Backend.Wallet                  (Wallet(..), toJS)
import qualified CSL
import           ENCOINS.App.Widgets.Basic       (elementResultJS)
import           ENCOINS.Crypto.Field            (Field(..), fromFieldElement, toFieldElement)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           JS.App                          (sha2_256, walletSignTx)
import           JS.Website                      (logInfo)
import           PlutusTx.Extra.ByteString       (ToBuiltinByteString(..), byteStringToInteger)

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "525c8a4bc4dc92461ef609f2cac7fd0bab5956cf8ce2162dbaac2f25"

getEncoinsInUtxos :: CSL.TransactionUnspentOutputs -> [Text]
getEncoinsInUtxos utxos = Map.keys assets
  where getMultiAsset (CSL.MultiAsset a) = a
        assets = foldr Map.union Map.empty
          (mapMaybe (Map.lookup encoinsCurrencySymbol . getMultiAsset) $
          mapMaybe (CSL.multiasset . CSL.amount . CSL.output) utxos)

mkRedeemer :: Address -> Address -> BulletproofParams -> Secrets -> [MintingPolarity] -> Randomness -> EncoinsRedeemer
mkRedeemer ledgerAddr changeAddr bp secrets mps rs = red
    where (v, inputs, proof) = bulletproof bulletproofSetup bp secrets mps rs
          inputs' = map (\(Input g p) -> (fromGroupElement g, p)) inputs
          sig = toBuiltin $ fromJust $
            decodeHex ""
          red = ((ledgerAddr, changeAddr), (v, inputs'), proof, sig)

calculateV :: Secrets -> [MintingPolarity] -> Integer
calculateV secrets mps = sum $ zipWith (\s mp -> fromFieldElement (secretV s) * polarityToInteger mp) secrets mps

mkAddress :: Address -> Integer -> Address
mkAddress addrWallet v = bool addrWallet ledgerAddress (v > 0)

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "58a04846566cd531318ee2e98e3044647f6c75e8224396515cc8aee9")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")

addressToBytes :: Address -> Text
addressToBytes (Address cr scr) = bs1 `Text.append` bs2
    where
        bs1 = encodeHex $ fromBuiltin $ case cr of
          PubKeyCredential (PubKeyHash pkh) -> pkh
          ScriptCredential (ValidatorHash vh) -> vh
        bs2 = encodeHex $ fromBuiltin $ case scr of
          Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> pkh
          Just (StakingHash (ScriptCredential (ValidatorHash vh))) -> vh
          _       -> emptyByteString

redeemerToBytes :: EncoinsRedeemer -> Text
redeemerToBytes ((aL, aC), i, p, _) = addressToBytes aL `Text.append` addressToBytes aC `Text.append`
  encodeHex (fromBuiltin $ toBytes i) `Text.append` encodeHex (fromBuiltin $ toBytes p)

encoinsTxWalletMode :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> Event t () ->
  m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxWalletMode dWallet dCoinsBurn dCoinsMint eSend = mdo
    baseUrl <- pabIP -- this chooses random server with successful ping
    let dAddrWallet = fmap walletChangeAddress dWallet
        dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs
        bWalletName = toJS . walletName <$> current dWallet

    -- Obtaining Secrets and [MintingPolarity]
    performEvent_ (logInfo "dCoinsBurn updated" <$ updated dCoinsBurn)
    performEvent_ (logInfo "dCoinsMint updated" <$ updated dCoinsMint)
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst
        -- dV       = zipDynWith calculateV  dSecrets dMPs
        -- dAddr    = zipDynWith mkAddress dAddrWallet dV

    -- Obtaining BulletproofParams
    dBulletproofParams <- elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)
    performEvent_ (flip sha2_256 "bulletproofParamsElement" . Text.append (addressToBytes ledgerAddress) . addressToBytes <$> updated dAddrWallet)

    -- Obtaining Randomness
    eRandomness <- performEvent $ liftIO randomIO <$ updated dBulletproofParams
    bRandomness <- hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer ledgerAddress <$> current dAddrWallet <*>
          current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- Constructing the final redeemer
    -- let bRedWithData = ffor2 bRed (current dAddrWallet) (\red a -> (a, red))
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eSend
    let eFinalRedeemer = () <$ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    (eNewTxSuccess, eRelayDown) <- newTxRequestWrapper baseUrl (zipDyn
      (fmap (Right . (,WalletMode) . fromJust) dFinalRedeemer)
      dInputs) eFinalRedeemer
    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    performEvent_ $ liftIO . logInfo . pack . show <$> eTx
    performEvent_ $ liftIO . logInfo . pack . show <$> eRelayDown

    -- Signing the transaction
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = () <$ updated dWalletSignature

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    (eSubmitted, eRelayDown') <- submitTxRequestWrapper baseUrl dSubmitReqBody eWalletSignature

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready      <$ eConfirmed,
          Balancing  <$ eFinalRedeemer,
          eRelayDown,
          Signing    <$ eNewTxSuccess,
          Submitting <$ eWalletSignature,
          eRelayDown',
          Submitted  <$ eSubmitted
          ]
    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)

encoinsTxTransferMode :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets
  -> Dynamic t [(Secret, Text)] -> Dynamic t (Maybe Address) -> Event t ()
  -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxTransferMode dWallet dCoins dNames dmAddr eSend = do
    baseUrl <- pabIP -- this chooses random server with successful ping
    let dUTXOs      = fmap walletUTXOs dWallet
        dInputs     = map CSL.input <$> dUTXOs
        dAddrWallet = fmap walletChangeAddress dWallet
        bWalletName = toJS . walletName <$> current dWallet
        dAddr       = fromMaybe ledgerAddress <$> dmAddr

    -- Constructing a new transaction
    (eNewTxSuccess, eRelayDown) <- newTxRequestWrapper baseUrl (zipDyn
      (fmap Left $ (,,) <$> dAddr <*> (zipDynWith mkValue dCoins dNames) <*>
        dAddrWallet) dInputs) eSend
    let eTxId = fmap fst eNewTxSuccess
        eTx   = fmap snd eNewTxSuccess
    dTx <- holdDyn "" eTx
    dTxId <- holdDyn "" eTxId

    performEvent_ $ liftIO . logInfo . pack . show <$> eTx
    performEvent_ $ liftIO . logInfo . pack . show <$> eRelayDown

    -- Signing the transaction
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    performEvent_ $ liftIO . walletSignTx <$> bWalletName `attach` eTx
    let eWalletSignature = () <$ updated dWalletSignature

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    (eSubmitted, eRelayDown') <- submitTxRequestWrapper baseUrl dSubmitReqBody eWalletSignature

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready      <$ eConfirmed,
          eRelayDown,
          Signing    <$ eNewTxSuccess,
          Submitting <$ eWalletSignature,
          eRelayDown',
          Submitted  <$ eSubmitted
          ]
    return (fmap getEncoinsInUtxos dUTXOs, eStatus, dTxId)
  where
    mkValue coins names = CSL.Value "1500000" . Just . CSL.MultiAsset . Map.singleton
      encoinsCurrencySymbol . Map.fromList . mapMaybe
        (\s -> (,"1") <$> lookup s names) $ coins

secretToHex :: Secret -> Text
secretToHex s = encodeHex . fromBuiltin $ toBytes $ gamma * 2^(20 :: Integer) + v
    where gamma = fromFieldElement $ secretGamma s
          v     = fromFieldElement $ secretV s

hexToSecret :: Text -> Maybe Secret
hexToSecret txt = do
    bs <- decodeHex txt
    let n = byteStringToInteger $ toBuiltin bs
        (gamma, v) = n `divMod` (2^(20 :: Integer))
    return $ Secret (toFieldElement gamma) (toFieldElement v)

encoinsTxLedgerMode :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> Event t () ->
  m (Dynamic t [Text], Event t Status)
encoinsTxLedgerMode dWallet dCoinsBurn dCoinsMint eSend = mdo
    baseUrl <- pabIP -- this chooses random server with successful ping
    ePb <- getPostBuild
    eTick <- tickLossyFromPostBuildTime 10
    (eStatusResp, eRelayDown') <- statusRequestWrapper baseUrl
      (pure LedgerEncoins) $ leftmost [ePb, void eTick]
    let
      toLedgerUtxoResult (LedgerUtxoResult xs) = Just xs
      toLedgerUtxoResult _ = Nothing
      eLedgerUtxoResult = mapMaybe toLedgerUtxoResult eStatusResp
    dUTXOs <- holdDyn [] eLedgerUtxoResult
    let
      dAddrWallet = fmap walletChangeAddress dWallet
      dInputs = map CSL.input <$> dUTXOs

    -- Obtaining Secrets and [MintingPolarity]
    performEvent_ (logInfo "dCoinsBurn updated" <$ updated dCoinsBurn)
    performEvent_ (logInfo "dCoinsMint updated" <$ updated dCoinsMint)
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst

    -- Obtaining BulletproofParams
    dBulletproofParams <- elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)
    performEvent_ (flip sha2_256 "bulletproofParamsElement" . Text.append (addressToBytes ledgerAddress) . addressToBytes <$> updated dAddrWallet)

    -- Obtaining Randomness
    eRandomness <- performEvent $ liftIO randomIO <$ updated dBulletproofParams
    bRandomness <- hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer ledgerAddress <$> current dAddrWallet <*>
          current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- Constructing the final redeemer
    -- let bRedWithData = ffor2 bRed (current dAddrWallet) (\red a -> (a, red))
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eSend
    let eFinalRedeemer = () <$ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    (eServerOk, eRelayDown) <- serverTxRequestWrapper baseUrl (zipDyn
      (fmap (Right . (,LedgerMode) . fromJust) dFinalRedeemer)
      dInputs) eFinalRedeemer
    performEvent_ $ liftIO . logInfo . pack . show <$> eRelayDown

    -- Tracking the pending transaction
    eConfirmed <- updated <$> holdUniqDyn dUTXOs

    let eStatus = leftmost [
          Ready      <$ eConfirmed,
          Balancing  <$ eFinalRedeemer,
          eRelayDown,
          Submitted  <$ eServerOk,
          eRelayDown' ]
    return (fmap getEncoinsInUtxos dUTXOs, eStatus)
