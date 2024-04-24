{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE JavaScriptFFI #-}

module Backend.Protocol.Types where

import Data.Aeson
    ( FromJSON (..)
    , FromJSONKey
    , ToJSON (..)
    , ToJSONKey
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    , tagSingleConstructors
    )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Version (Version)
import GHC.Generics (Generic)
import PlutusTx.Builtins
import Reflex.Dom (decodeText)
import Text.Hex (decodeHex, encodeHex)

import CSL (TransactionUnspentOutputs, Value)
import ENCOINS.BaseTypes (MintingPolarity)
import ENCOINS.Bulletproofs (Proof, Secret)

type TxParams = (Address, Address, Integer)

-- Defining Address data type for sending it as JSON
data Address = Address
    { addressCredential :: Credential
    , addressStakingCredential :: Maybe StakingCredential
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data Credential = PubKeyCredential PubKeyHash | ScriptCredential ValidatorHash
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data StakingCredential = StakingHash Credential | StakingPtr Integer Integer Integer
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype PubKeyHash = PubKeyHash {getPubKeyHash :: BuiltinByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype ValidatorHash = ValidatorHash BuiltinByteString
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON)

mkAddressFromPubKeys :: Text -> Maybe Text -> Address
mkAddressFromPubKeys pkhHex mskhHex =
    Address
        (PubKeyCredential $ PubKeyHash pkh)
        (StakingHash . PubKeyCredential . PubKeyHash <$> mskh)
    where
        pkh = toBuiltin $ fromJust $ decodeHex pkhHex
        mskh = toBuiltin <$> (mskhHex >>= decodeHex)

addressToBytes :: Address -> Text
addressToBytes (Address cr scr) = bs1 `Text.append` bs2
    where
        bs1 = encodeHex $ fromBuiltin $ case cr of
            PubKeyCredential (PubKeyHash pkh) -> pkh
            ScriptCredential (ValidatorHash vh) -> vh
        bs2 = encodeHex $ fromBuiltin $ case scr of
            Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> pkh
            Just (StakingHash (ScriptCredential (ValidatorHash vh))) -> vh
            _ -> emptyByteString

checkEmptyText :: Text -> Maybe Text
checkEmptyText "" = Nothing
checkEmptyText txt = Just txt

type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

data InputOfEncoinsApi
    = InputRedeemer EncoinsRedeemer EncoinsMode
    | InputSending Address Value Address
    | InputDelegation Address Text
    deriving (Show, Generic, FromJSON, ToJSON)

data Witness = Witness {vkey :: Text, signature :: Text}
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON)

decodeWitness :: Text -> [(Text, Text)]
decodeWitness = maybe [] (map (\(Witness k s) -> (k, s))) . decodeText

newtype PubKey = PubKey {getPubKey :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Signature = Signature {getSignature :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SubmitTxReqBody = SubmitTxReqBody
    { submitReqTx :: Text
    , submitReqWitnesses :: [(Text, Text)]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data EncoinsMode = WalletMode | TransferMode | LedgerMode
    deriving stock (Eq, Read, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncoinsStatusReqBody = MaxAdaWithdraw | LedgerEncoins
    deriving stock (Eq, Enum, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncoinsStatusResult
    = MaxAdaWithdrawResult Integer
    | LedgerUtxoResult TransactionUnspentOutputs
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

showStatus :: EncoinsStatusResult -> Text
showStatus (MaxAdaWithdrawResult n) = "MaxAdaWithdrawResult: " <> Text.pack (show n)
showStatus (LedgerUtxoResult t) = "LedgerUtxoResult: " <> Text.pack (show $ length t)

data ServerVersion = ServerVersion
    { svVersion :: Version
    , svCommit :: Text
    , svDate :: UTCTime
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PasswordRaw = PasswordRaw {getPassRaw :: Text} deriving (Eq, Show)

-- Save types

-- Secret hash is used to save
newtype EncryptedSecret = MkEncryptedSecret {getEncryptedSecret :: Text}
    deriving newtype (Eq, Show, ToJSON, FromJSON)
    deriving stock (Generic)

newtype AssetName = MkAssetName {getAssetName :: Text}
    deriving newtype (Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    deriving stock (Generic)

-- Request body from frontend to backend
data SaveRequest = MkSaveRequest
    { srAssetName :: AssetName
    , srSecretKey :: EncryptedSecret
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON SaveRequest where
    toJSON = genericToJSON $ aesonPrefix snakeCase

data StatusResponse = MkStatusResponse
    { spStatusResponse :: SaveStatus
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON StatusResponse where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- Client sets SaveUndefined status only
-- Server sets all other statuses
data SaveStatus = SaveUndefined | SaveError | Saved | Discarded
    deriving stock (Show, Eq, Generic)

instance ToJSON SaveStatus where
    toJSON =
        genericToJSON $
            defaultOptions{tagSingleConstructors = True}

instance FromJSON SaveStatus where
    parseJSON =
        genericParseJSON $
            defaultOptions{tagSingleConstructors = True}

data RestoreResponse = MkRestoreResponse
    { rrAssetName :: AssetName
    , rrEncryptedSecret :: EncryptedSecret
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON RestoreResponse where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- Randomly generated aes256 key for encrypting Secrets before saving them
newtype AesKeyRaw = MkAesKeyRaw {getAesKeyRaw :: Text}
    deriving newtype (Eq, Show, ToJSON, FromJSON)
    deriving stock (Generic)

-- Cache

data TokenCacheV3 = MkTokenCacheV3
    { tcAssetName :: AssetName
    , tcSecret :: Secret
    , tcSaveStatus :: SaveStatus
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON TokenCacheV3 where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON TokenCacheV3 where
    toJSON = genericToJSON $ aesonPrefix snakeCase

-- For readable logs
showToken :: TokenCacheV3 -> (AssetName, SaveStatus)
showToken (MkTokenCacheV3 n _ i) = (n, i)

showTokens ::
    (Foldable t, Functor t) => t TokenCacheV3 -> t (AssetName, SaveStatus)
showTokens = fmap showToken
