module Backend.EncoinsTx where

import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.Aeson                      (decode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.FileEmbed                  (embedFile)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text, pack)
import qualified Data.Text                       as Text
import           PlutusTx.Builtins
import           Reflex.Dom                      hiding (Input)
import           System.Random.Stateful          (randomIO)
import           Text.Hex                        (encodeHex, decodeHex)
import           Witherable                      (catMaybes)

import           Backend.Servant.Client          (pabIP)
import           Backend.Servant.Requests        (submitTxRequestWrapper, newTxRequestWrapper)
import           Backend.Status                  (Status (..))
import           Backend.Types
import           Backend.Wallet                  (Wallet(..), toJS)
import qualified CSL
import           ENCOINS.App.Widgets.Basic       (elementResultJS)
import           ENCOINS.Crypto.Field            (Field(..), fromFieldElement)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           JS.App                          (sha2_256, walletSignTx)
import           JS.Website                      (logInfo)
import           PlutusTx.Extra.ByteString       (ToBuiltinByteString(..))

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "7ed5a24ee4932199aee74b148229b3dc1bcb7f5bd1db30c4763a768c"


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
mkAddress addrWallet v = bool addrWallet addrVal (v > 0)
    where addrVal = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
            decodeHex "7677307197342cb313c7b8417643516dd6ebd419b34d4543ba70f708")
            (Just $ StakingHash $ ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
            decodeHex "662cf4fedcd340134a8a7c0a38411a4b8bc9e6b76a6ef706f00756bc")

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

encoinsTx :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> Event t () ->
  m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTx dWallet dCoinsBurn dCoinsMint eSend = mdo
    baseUrl <- pabIP -- this chooses random server with successful ping
    let dAddrWallet = fmap walletChangeAddress dWallet
        dUTXOs      = fmap walletUTXOs dWallet
        bWalletName = toJS . walletName <$> current dWallet

    -- Obtaining Secrets and [MintingPolarity]
    performEvent_ (logInfo "dCoinsBurn updated" <$ updated dCoinsBurn)
    performEvent_ (logInfo "dCoinsMint updated" <$ updated dCoinsMint)
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst
        -- dV       = zipDynWith calculateV  dSecrets dMPs
        -- dAddr    = zipDynWith mkAddress dAddrWallet dV
        ledgerAddr = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
            decodeHex "7677307197342cb313c7b8417643516dd6ebd419b34d4543ba70f708")
            (Just $ StakingHash $ ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
            decodeHex "662cf4fedcd340134a8a7c0a38411a4b8bc9e6b76a6ef706f00756bc")

    -- Obtaining BulletproofParams
    dBulletproofParams <- elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)
    performEvent_ (flip sha2_256 "bulletproofParamsElement" . Text.append (addressToBytes ledgerAddr) . addressToBytes <$> updated dAddrWallet)

    -- Obtaining Randomness
    eRandomness <- performEvent $ liftIO randomIO <$ updated dBulletproofParams
    bRandomness <- hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer ledgerAddr <$> current dAddrWallet <*>
          current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- Constructing the final redeemer
    -- let bRedWithData = ffor2 bRed (current dAddrWallet) (\red a -> (a, red))
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRed `tag` eSend
    let eFinalRedeemer = () <$ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    (eNewTxSuccess, eRelayDown) <- newTxRequestWrapper baseUrl (zipDyn (fmap fromJust dFinalRedeemer) dUTXOs) eFinalRedeemer
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

encoinsTxWallet :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets
  -> Event t () -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxWallet _dWallet _dCoins eSend = do
    _baseUrl <- pabIP -- this chooses random server with successful ping
    performEvent_ (logInfo "encoinsTxWallet" <$ eSend)
    return (pure [], Submitting <$ eSend, pure "")

encoinsTxLedger :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets
  -> Event t () -> m (Dynamic t [Text], Event t Status, Dynamic t Text)
encoinsTxLedger _dWallet _dCoins eSend = do
    _baseUrl <- pabIP -- this chooses random server with successful ping
    performEvent_ (logInfo "encoinsTxLedger" <$ eSend)
    return (pure [], Submitting <$ eSend, pure "")
