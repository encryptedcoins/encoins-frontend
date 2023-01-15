module Backend.EncoinsTx where

import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.Aeson                      (decode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.FileEmbed                  (embedFile)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           PlutusTx.Builtins
import           Reflex.Dom                      hiding (Input)
import           System.Random.Stateful          (randomIO)
import           Text.Hex                        (encodeHex, decodeHex)
import           Witherable                      (catMaybes)

import           Backend.Servant.Requests        (submitTxRequestWrapper)
import           Backend.Types
import           CSL                             (TransactionUnspentOutputs)
import           ENCOINS.Crypto.Field            (Field(..), fromFieldElement)
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           JS.App                          (walletLoad, sha2_256, ed25519Sign)
import           PlutusTx.Extra.ByteString       (ToBuiltinByteString(..))
import           Widgets.Basic                   (elementResultJS)
import           Widgets.Events                  (newEvent)

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

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
            decodeHex "c1a6c70cbc8f532a93c4639b3541715a1f949444a8923be41382c01b") Nothing

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

encoinsTx :: MonadWidget t m => Dynamic t Secrets -> Dynamic t Secrets -> m ()
encoinsTx dCoinsBurn dCoinsMint = mdo
    e <- newEvent

    -- TODO: implement wallet switcher
    -- dWalletName <- holdDyn "nami" never

    -- TODO: add wallet enable functionality
    -- let parseConnected b = bool False True (b == "true")
    -- dWalletConnected <- fmap (fmap parseConnected . value) $ inputElement $ def
    --   & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId)
    --   & inputElementConfig_initialValue .~ "false"

    -- Retrieving wallet info
    dPubKeyHash <- elementResultJS "pubKeyHashElement" id
    dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
    dUTXOs <- elementResultJS "utxosElement" (fromJust . decodeText :: Text -> TransactionUnspentOutputs)
    let dAddrWallet =  zipDynWith mkAddressFromPubKeys dPubKeyHash dStakeKeyHash
    performEvent_ (walletLoad "eternl" "" "" "" "" "pubKeyHashElement" "stakeKeyHashElement" "" "utxosElement" "" "" <$ e)

    -- Obtaining Secrets and [MintingPolarity]
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

    -- Button that sends the request
    (elButton, _) <- el' "button" $ text "Click me!"
    let eSend = domEvent Click elButton

    -- Obtaining EncoinsRedeemer
    let bRed = mkRedeemer <$> current dAddr <*> current dBulletproofParams <*> current dSecrets <*> current dMPs <*> bRandomness

    -- NOTE: for testing purposes we sign the redeemer on frontend
    -- Obtaining redeemer hash
    dRedeemerHash <- elementResultJS "redeemerHashElement" id
    performEvent_ (flip sha2_256 "redeemerHashElement" . redeemerToBytes <$> bRed `tag` eSend)

    -- Obtaining signature
    dSig <- elementResultJS "ed25519SigElement" (toBuiltin . fromJust . decodeHex)
    let prvKey = "1DA4194798C1D3AA8B7E5E39EDA1F130D9123ACCC8CA31A82E033A6D007DA7EC"
    performEvent_ (flip (ed25519Sign prvKey) "ed25519SigElement" <$> updated dRedeemerHash)
    let eSig = updated dSig

    -- Constructing the final redeemer
    let bRedWithData = ffor2 bRed (current dAddrWallet) (\red a -> (a, red))
    dFinalRedeemer <- holdDyn Nothing $ Just <$> attachWith (\(ca, (a, i, p, _)) s -> (ca, (a, i, p, s))) bRedWithData eSig
    let eFinalRedeemer = () <$ catMaybes (updated dFinalRedeemer)

    -- Constructing transaction
    -- eTx <- newTxRequestWrapper (fromJust <$> dFinalRedeemer) dUTXOs eFinalRedeemer

    -- Signing transaction
    -- dWalletSignature <- elementResultJS "walletSignatureElement" id
    -- performEvent_ $ liftIO . flip (walletSignTx "eternl") "walletSignatureElement" <$> eTx

    -- Submitting transaction
    _ <- submitTxRequestWrapper (fromJust <$> dFinalRedeemer) dUTXOs eFinalRedeemer

    blank