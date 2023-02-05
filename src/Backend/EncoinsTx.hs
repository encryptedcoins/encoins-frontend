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

import           Backend.Servant.Requests        (submitTxRequestWrapper, newTxRequestWrapper)
import           Backend.Types
import           Backend.Wallet                  (Wallet(..))
import qualified CSL
import           ENCOINS.Crypto.Field            (Field(..), fromFieldElement)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           JS.App                          (sha2_256, walletSignTx)
import           JS.Website                      (logInfo)
import           PlutusTx.Extra.ByteString       (ToBuiltinByteString(..))
import           Widgets.Basic                   (elementResultJS)

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "40fd7027a6e67e85f5b15fbf98818af339b2c7cdb1283115d51d6be9"

getEncoinsInUtxos :: CSL.TransactionUnspentOutputs -> [Text]
getEncoinsInUtxos utxos = maybe [] Map.keys assets
  where getMultiAsset (CSL.MultiAsset a) = a
        assets = Map.lookup encoinsCurrencySymbol $ foldr (Map.union . getMultiAsset)
                  Map.empty (mapMaybe (CSL.multiasset . CSL.amount . CSL.output) utxos)

mkRedeemer :: Address -> BulletproofParams -> Secrets -> [MintingPolarity] -> Randomness -> EncoinsRedeemer
mkRedeemer addr bp secrets mps rs = red
    where (v, inputs, proof) = bulletproof bulletproofSetup bp secrets mps rs
          inputs' = map (\(Input g p) -> (fromGroupElement g, p)) inputs
          sig = toBuiltin $ fromJust $
            decodeHex "AF26CA8095F1CD9996237878DDF2380FAAF64D045D1C7B1EBB6F1C25F450FA88D4AEBED827878FD8ACEF7C7501C4E2C1884F5FFB8C3ADED7CD607A7425705D0A"
          red = (addr, (v, inputs'), proof, sig)

calculateV :: Secrets -> [MintingPolarity] -> Integer
calculateV secrets mps = sum $ zipWith (\s mp -> fromFieldElement (secretV s) * polarityToInteger mp) secrets mps

mkAddress :: Address -> Integer -> Address
mkAddress addrWallet v = bool addrWallet addrVal (v > 0)
    where addrVal = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
            decodeHex "201954c6f771e459e6588c06d701bcdd5216fb54cc1c1a7f5e037e9b") Nothing

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
redeemerToBytes (a, i, p, _) = addressToBytes a `Text.append` encodeHex (fromBuiltin $ toBytes i) `Text.append` encodeHex (fromBuiltin $ toBytes p)

encoinsTx :: MonadWidget t m => Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> Event t () -> m (Dynamic t [Text])
encoinsTx dWallet dCoinsBurn dCoinsMint eSend = mdo
    let dAddrWallet = fmap walletChangeAddress dWallet
        dUTXOs      = fmap walletUTXOs dWallet

    -- Obtaining Secrets and [MintingPolarity]
    performEvent_ (logInfo "dCoinsBurn updated" <$ updated dCoinsBurn)
    performEvent_ (logInfo "dCoinsMint updated" <$ updated dCoinsMint)
    let dLst = unzip <$> zipDynWith (++) (fmap (map (, Burn)) dCoinsBurn) (fmap (map (, Mint)) dCoinsMint)
        dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst
        dV       = zipDynWith calculateV  dSecrets dMPs
        dAddr    = zipDynWith mkAddress dAddrWallet dV

    -- Obtaining BulletproofParams
    dBulletproofParams <- elementResultJS "bulletproofParamsElement" (parseBulletproofParams . toBuiltin . fromJust . decodeHex)
    performEvent_ (flip sha2_256 "bulletproofParamsElement" . addressToBytes <$> updated dAddr)

    -- Obtaining Randomness
    eRandomness <- performEvent $ liftIO randomIO <$ updated dBulletproofParams
    bRandomness <- hold (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer <$> current dAddr <*> current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- Constructing the final redeemer
    let bRedWithData = ffor2 bRed (current dAddrWallet) (\red a -> (a, red))
    dFinalRedeemer <- holdDyn Nothing $ Just <$> bRedWithData `tag` eSend
    let eFinalRedeemer = () <$ catMaybes (updated dFinalRedeemer)

    -- Constructing a new transaction
    eTx <- newTxRequestWrapper (zipDyn (fmap fromJust dFinalRedeemer) dUTXOs) eFinalRedeemer
    dTx <- holdDyn "" eTx

    -- Signing the transaction
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    performEvent_ $ liftIO . flip (walletSignTx "eternl") "walletSignatureElement" <$> eTx
    let eWalletSignature = () <$ updated dWalletSignature
    performEvent_ $ liftIO . logInfo . pack . show <$> updated dWalletSignature

    -- Submitting the transaction
    let dSubmitReqBody = zipDynWith SubmitTxReqBody dTx dWalletSignature
    _ <- submitTxRequestWrapper dSubmitReqBody eWalletSignature

    return $ fmap getEncoinsInUtxos dUTXOs