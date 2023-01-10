{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module JS.Types where

import           Data.Aeson                  (ToJSON(..), FromJSON (parseJSON), ToJSONKey, FromJSONKey, withObject, (.:))
import           Data.ByteString             (ByteString)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, pack)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (ToJSVal(..))
import           PlutusTx.Builtins
import           Text.Hex                    (encodeHex, decodeHex)

import           ENCOINS.BaseTypes           (MintingPolarity, GroupElement (..))
import           ENCOINS.Bulletproofs        (Proof (..))
import           ENCOINS.Crypto.Field        (Field(..))

type AddressBech32 = Text

type TxParamsFrontend = AddressBech32
type TxParams = Address

-- Defining Address data type for sending it as JSON
data Address = Address{ addressCredential :: Credential, addressStakingCredential :: Maybe StakingCredential }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
data Credential = PubKeyCredential PubKeyHash | ScriptCredential ScriptHash
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
data StakingCredential = StakingHash Credential | StakingPtr Integer Integer Integer
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
newtype PubKeyHash = PubKeyHash { getPubKeyHash :: BuiltinByteString }
    deriving stock (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON)
newtype ScriptHash = ScriptHash { getScriptHash :: BuiltinByteString }
    deriving stock (Generic, Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

mkAddressFromPubKeys :: Text -> Text -> Address
mkAddressFromPubKeys pkhHex skhHex = Address (PubKeyCredential $ PubKeyHash pkh) (Just $ StakingHash $ PubKeyCredential $ PubKeyHash skh)
    where pkh = toBuiltin $ fromJust $ decodeHex pkhHex
          skh = toBuiltin $ fromJust $ decodeHex skhHex

type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemerFrontend = (TxParamsFrontend, EncoinsInput, Proof, ProofSignature)
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

newtype EncoinsRedeemerJS = EncoinsRedeemerJS EncoinsRedeemerFrontend
    deriving (Eq, Show, Generic, ToJSVal)

instance ToJSVal Integer where
    toJSVal = toJSVal . pack . show

instance ToJSVal BuiltinByteString where
    toJSVal = toJSVal . pack . show

instance ToJSVal MintingPolarity where
    toJSVal = toJSVal . pack . show

instance ToJSVal (Field c) where
    toJSVal (F a) = toJSVal a

instance ToJSVal GroupElement where
    toJSVal (GroupElement (x, y)) = toJSVal (x, y)

instance ToJSVal Proof where
    toJSVal (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) = toJSVal ([commitA, commitS, commitT1, commitT2], [taux, mu, tHat], lx, rx)

newtype TxId = TxId { getTxId :: BuiltinByteString }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data TxOutRef = TxOutRef {
    txOutRefId  :: TxId, -- ^ The transaction ID.
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON)

instance FromJSON TxOutRef where
    parseJSON = withObject "TxOutRef" $ \v -> do
        txId  <- v .: "transaction_id"
        txIdx <- v .: "index"
        return $ TxOutRef (TxId txId) txIdx

data DecoratedTxOut =
    PublicKeyDecoratedTxOut {
      -- | The pubKey hash that protects the transaction address
      _decoratedTxOutPubKeyHash        :: PubKeyHash,
      -- | The staking credential of the transaction address, if any
      _decoratedTxOutStakingCredential :: Maybe StakingCredential,
      -- | Value of the transaction output.
      _decoratedTxOutValue             :: Value,
      -- | Optional datum (inline datum or datum in transaction body) attached to the transaction output.
      _decoratedTxOutPubKeyDatum       :: Maybe (DatumHash, DatumFromQuery),
      -- | Value of the transaction output.
      _decoratedTxOutReferenceScript   :: Maybe (Versioned Script)
    }
    | ScriptDecoratedTxOut {
      -- | The hash of the script that protects the transaction address
      _decoratedTxOutValidatorHash     :: ValidatorHash,
      -- | The staking credential of the transaction address, if any
      _decoratedTxOutStakingCredential :: Maybe StakingCredential,
      -- | Value of the transaction output.
      _decoratedTxOutValue             :: Value,
      -- | Datum attached to the transaction output, either in full (inline datum or datum in transaction body) or as a
      -- hash reference. A transaction output protected by a Plutus script
      -- is guardateed to have an associated datum.
      _decoratedTxOutScriptDatum       :: (DatumHash, DatumFromQuery),
      -- The reference script is, in genereal, unrelated to the validator
      -- script althought it could also be the same.
      _decoratedTxOutReferenceScript   :: Maybe (Versioned Script),
      -- | Full version of the validator protecting the transaction output
      _decoratedTxOutValidator         :: Maybe (Versioned Validator)
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

-- instance FromJSON DecoratedTxOut where
--     parseJSON = withObject "PublicKeyDecoratedTxOut" $ \v -> do
--         pkh  <- v .: "transaction_id"
--         txIdx <- v .: "index"
--         return $ DecoratedTxOut pkh skh val Nothing Nothing

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: BuiltinByteString }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype TokenName = TokenName { unTokenName :: BuiltinByteString }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data DatumFromQuery
    = DatumUnknown
    | DatumInline Datum
    | DatumInBody Datum
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

type Datum = ()
-- newtype Datum = Datum { getDatum :: BuiltinData  }
--     deriving stock (Eq, Ord, Show, Generic)
--     deriving (ToJSON, FromJSON)

newtype DatumHash = DatumHash BuiltinByteString
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype ValidatorHash = ValidatorHash BuiltinByteString
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype Script = Script { unScript :: BuiltinByteString }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype Validator = Validator { getValidator :: Script }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data Language = PlutusV1 | PlutusV2
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data Versioned script = Versioned { unversioned :: script, version :: Language }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)