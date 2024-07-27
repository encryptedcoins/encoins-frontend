module Common.Protocol where 

import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import PlutusTx.Builtins
import Text.Hex (decodeHex, encodeHex)

import Backend.Protocol.Setup
    ( bulletproofSetup
    , encoinsCurrencySymbol
    , ledgerAddress
    )
import Backend.Protocol.Types
import qualified CSL
import ENCOINS.BaseTypes
import ENCOINS.Bulletproofs
import ENCOINS.Crypto.Field (fromFieldElement, toFieldElement)
import PlutusTx.Extra.ByteString (ToBuiltinByteString (..), byteStringToInteger)

getEncoinsInUtxos :: CSL.TransactionUnspentOutputs -> [AssetName]
getEncoinsInUtxos utxos = map MkAssetName $ Map.keys assets
    where
        getMultiAsset (CSL.MultiAsset a) = a
        assets =
            foldr
                Map.union
                Map.empty
                ( mapMaybe (Map.lookup encoinsCurrencySymbol . getMultiAsset) $
                    mapMaybe (CSL.multiasset . CSL.amount . CSL.output) utxos
                )

verifyRedeemer :: BulletproofParams -> Maybe EncoinsRedeemer -> Bool
verifyRedeemer bp (Just (_, (v, inputs), proof, _)) = verify bulletproofSetup bp v inputs' proof
    where
        inputs' = map (\(bs, p) -> Input (fromJust $ toGroupElement bs) p) inputs
verifyRedeemer _ _ = False

calculateV :: Secrets -> [MintingPolarity] -> Integer
calculateV secrets mps =
    sum $
        zipWith
            (\s mp -> fromFieldElement (secretV s) * polarityToInteger mp)
            secrets
            mps

mkAddress :: Address -> Integer -> Address
mkAddress addrWallet v = bool addrWallet ledgerAddress (v > 0)

redeemerToBytes :: EncoinsRedeemer -> Text
redeemerToBytes ((_, aC, fees), i, p, _) =
    addressToBytes aC
        `Text.append` encodeHex (fromBuiltin $ toBytes fees)
        `Text.append` encodeHex (fromBuiltin $ toBytes i)
        `Text.append` encodeHex (fromBuiltin $ toBytes p)

secretToHex :: Secret -> Text
secretToHex s = encodeHex . fromBuiltin $ toBytes $ gamma * 2 ^ (20 :: Integer) + v
    where
        gamma = fromFieldElement $ secretGamma s
        v = fromFieldElement $ secretV s

hexToSecret :: Text -> Maybe Secret
hexToSecret = \case
    "" -> Nothing
    txt -> do
        bs <- decodeHex txt
        let n = byteStringToInteger $ toBuiltin bs
            (gamma, v) = n `divMod` (2 ^ (20 :: Integer))
        return $ Secret (toFieldElement gamma) (toFieldElement v)

isMultiAssetOf :: Text -> Text -> CSL.MultiAsset -> Bool
isMultiAssetOf symbol token (CSL.MultiAsset mp) =
    case Map.lookup symbol mp of
        Nothing -> False
        Just i -> maybe False (const True) $ Map.lookup token i