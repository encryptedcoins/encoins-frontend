module Backend.EncoinsTx where

import           Control.Monad.IO.Class    (MonadIO(..))
import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.FileEmbed            (embedFile)
import           Data.Maybe                (fromJust)
import           Data.Text                 (pack)
import qualified Data.Text                 as Text
import           PlutusTx.Builtins
import           Reflex.Dom                hiding (Input)
import           System.Random.Stateful    (randomIO)
import           Text.Hex                  (encodeHex, decodeHex)

import           Backend.Requests
import           ENCOINS.Crypto.Field      (Field(..))
import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           JS.App                    (walletLoad, sha2_256, ed25519Sign)
import           JS.Types                  (EncoinsRedeemerFrontend, AddressBech32)
import           JS.WebPage                (logInfo)
import           PlutusTx.Extra.ByteString (ToBuiltinByteString(..))
import           Reflex.ScriptDependent    (widgetHoldUntilDefined)
import           Widgets.Basic             (elementResultJS)
import           Widgets.CoinEntry         (coinCollectionWidget)

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

mkRedeemer :: AddressBech32 -> BulletproofParams -> Secrets -> [MintingPolarity] -> Randomness -> EncoinsRedeemerFrontend
mkRedeemer addrBech32 bp secrets mps rs = red
    where (v, inputs, proof) = bulletproof bulletproofSetup bp secrets mps rs
          inputs' = map (\(Input g p) -> (fromGroupElement g, p)) inputs
          sig = toBuiltin $ fromJust $ decodeHex "AF26CA8095F1CD9996237878DDF2380FAAF64D045D1C7B1EBB6F1C25F450FA88D4AEBED827878FD8ACEF7C7501C4E2C1884F5FFB8C3ADED7CD607A7425705D0A"
          red = (addrBech32, (v, inputs'), proof, sig)

encoinsTx :: MonadWidget t m => m ()
encoinsTx = mdo
    ePb <- getPostBuild
    eEncoinsLoaded <- updated <$> widgetHoldUntilDefined "walletEnable" ("js/ENCOINS.js" <$ ePb) blank blank

    -- TODO: implement wallet switcher
    -- dWalletName <- holdDyn "nami" never

    -- TODO: add wallet enable functionality
    -- let parseConnected b = bool False True (b == "true")
    -- dWalletConnected <- fmap (fmap parseConnected . value) $ inputElement $ def
    --   & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId)
    --   & inputElementConfig_initialValue .~ "false"

    -- Retrieving wallet info
    _  <- elementResultJS "changeAddressElement" id
    dWalletAddressBech32 <- elementResultJS "changeAddressBech32Element" id
    dPubKeyHash <- elementResultJS "pubKeyHashElement" id
    dStakeKeyHash <- elementResultJS "stakeKeyHashElement" id
    let dAddrBytes = zipDynWith Text.append dPubKeyHash dStakeKeyHash
    performEvent_ (walletLoad "nami" "" "" "changeAddressElement" "changeAddressBech32Element"
        "pubKeyHashElement" "stakeKeyHashElement" "" "" "" "" <$ eEncoinsLoaded)

    -- Defining bulletproof algorithm arguments
    dBulletproofParams <- elementResultJS "bulletproofParamsElement" (fromJust . toGroupElement . toBuiltin . fromJust . decodeHex)
    performEvent_ (flip sha2_256 "bulletproofParamsElement" <$> updated dAddrBytes)
    dLst <- fmap unzip <$> coinCollectionWidget
    let dSecrets = fmap fst dLst
        dMPs     = fmap snd dLst
    eRandomness <- performEvent $ liftIO randomIO <$ eEncoinsLoaded
    dRandomness <- holdDyn (Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)) eRandomness

    -- Obtaining EncoinsRedeemer
    let dRed = mkRedeemer <$> dWalletAddressBech32 <*> dBulletproofParams <*> dSecrets <*> dMPs <*> dRandomness

    -- NOTE: for testing purposes we sign the redeemer on frontend
    -- Obtaining redeemer hash
    let redToBytes (a, i, p, _) = a `Text.append` encodeHex (fromBuiltin $ toBytes i) `Text.append` encodeHex (fromBuiltin $ toBytes p)
    dRedeemerHash <- elementResultJS "redeemerHashElement" id
    performEvent_ (flip sha2_256 "redeemerHashElement" <$> updated (fmap redToBytes dRed))
    -- Obtaining signature
    dSig <- elementResultJS "ed25519SigElement" (toBuiltin . fromJust . decodeHex)
    let prvKey = "1DA4194798C1D3AA8B7E5E39EDA1F130D9123ACCC8CA31A82E033A6D007DA7EC"
    performEvent_ (flip (ed25519Sign prvKey) "ed25519SigElement" <$> updated dRedeemerHash)

    -- Constructing the final redeemer
    let dFinalRedeemer = zipDynWith (\(a, i, p, _) s -> (a, i, p, s)) dRed dSig

    performEvent_ $ logInfo . pack . show <$> updated dFinalRedeemer
    _ <- newEncoinsTxRequest dFinalRedeemer

    -- TODO: add transaction submit logic
    -- _ <- elementResultJS "testResultElement" id
    -- performEvent_ $ liftIO . encoinsTxSubmit "nami" "" red <$> ("testResultElement" <$ eEncoinsLoaded)

    blank