{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE JavaScriptFFI  #-}

module Backend.Protocol.Types where

import           Data.Aeson           (FromJSON (..),
                                       Options (fieldLabelModifier),
                                       ToJSON (..), camelTo2, defaultOptions,
                                       genericParseJSON, withObject, (.:))
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           Data.Version         (Version)
import           GHC.Generics         (Generic)
import           PlutusTx.Builtins
import           Reflex.Dom           (decodeText)
import           Text.Hex             (decodeHex, encodeHex)

import           CSL                  (TransactionUnspentOutputs, Value)
import           ENCOINS.BaseTypes    (MintingPolarity)
import           ENCOINS.Bulletproofs (Proof)

type TxParams = (Address, Address, Integer)

-- Defining Address data type for sending it as JSON
data Address = Address{ addressCredential :: Credential, addressStakingCredential :: Maybe StakingCredential }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
data Credential = PubKeyCredential PubKeyHash | ScriptCredential ValidatorHash
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
data StakingCredential = StakingHash Credential | StakingPtr Integer Integer Integer
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)
newtype PubKeyHash = PubKeyHash { getPubKeyHash :: BuiltinByteString }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
newtype ValidatorHash = ValidatorHash BuiltinByteString
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON)

mkAddressFromPubKeys :: Text -> Maybe Text -> Address
mkAddressFromPubKeys pkhHex mskhHex = Address (PubKeyCredential $ PubKeyHash pkh)
    (StakingHash . PubKeyCredential . PubKeyHash <$> mskh)
    where pkh = toBuiltin $ fromJust $ decodeHex pkhHex
          mskh = toBuiltin <$> (mskhHex >>= decodeHex)

addressToBytes :: Address -> Text
addressToBytes (Address cr scr) = bs1 `Text.append` bs2
    where
        bs1 = encodeHex $ fromBuiltin $ case cr of
          PubKeyCredential (PubKeyHash pkh)   -> pkh
          ScriptCredential (ValidatorHash vh) -> vh
        bs2 = encodeHex $ fromBuiltin $ case scr of
          Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> pkh
          Just (StakingHash (ScriptCredential (ValidatorHash vh))) -> vh
          _       -> emptyByteString

checkEmptyText :: Text -> Maybe Text
checkEmptyText ""  = Nothing
checkEmptyText txt = Just txt

type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

data InputOfEncoinsApi
    = InputRedeemer   EncoinsRedeemer EncoinsMode
    | InputSending    Address Value Address
    | InputDelegation Address Text
    deriving (Show, Generic, FromJSON, ToJSON)

data Witness = Witness { vkey :: Text, signature :: Text }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON)

decodeWitness :: Text -> [(Text, Text)]
decodeWitness = maybe [] (map (\(Witness k s) -> (k, s))) . decodeText

newtype PubKey = PubKey { getPubKey :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Signature = Signature { getSignature :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SubmitTxReqBody = SubmitTxReqBody
    {
        submitReqTx        :: Text,
        submitReqWitnesses :: [(Text, Text)]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data EncoinsMode = WalletMode | TransferMode | LedgerMode
    deriving stock (Eq, Read, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncoinsStatusReqBody = MaxAdaWithdraw | LedgerEncoins
    deriving stock (Eq, Enum, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncoinsStatusResult = MaxAdaWithdrawResult Integer | LedgerUtxoResult TransactionUnspentOutputs
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

showStatus :: EncoinsStatusResult -> Text
showStatus (MaxAdaWithdrawResult n) = "MaxAdaWithdrawResult: " <> Text.pack (show n)
showStatus (LedgerUtxoResult t) = "LedgerUtxoResult: " <> Text.pack (show $ length t)

data ServerVersion = ServerVersion
  { svVersion :: Version
  , svCommit  :: Text
  , svDate    :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- IPFS

-- TODO Person is just for tests. Remove it later.
data Person = Person
  { name :: Text
  , age  :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PinJsonResponse = MkPinJsonResponse
  { ipfsHash    :: Text
  , pinSize     :: Int
  , timestamp   :: UTCTime
  , isDuplicate :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PinJsonResponse where
    parseJSON = withObject "PinJsonResponse" $ \o -> do
        ipfsHash    <- o .: "IpfsHash"
        pinSize     <- o .: "PinSize"
        timestamp   <- o .: "Timestamp"
        isDuplicate <- o .: "isDuplicate"
        pure MkPinJsonResponse{..}

data File = MkFile
  { fileId        :: Text
  , ipfsPinHash   :: Text
  , size          :: Int
  , userId        :: Text
  , datePinned    :: Maybe UTCTime
  , dateUnpinned  :: Maybe UTCTime
  , metadata      :: FileMetadata
  , regions       :: [Regions]
  , mimeType      :: Text
  , numberOfFiles :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON File where
    parseJSON = genericParseJSON
      defaultOptions{ fieldLabelModifier = modifyField }

modifyField :: String -> String
modifyField "fileId" = "id"
modifyField key      = camelTo2 '_' key

data Regions = MkRegions
  { regionId                :: Text
  , currentReplicationCount :: Int
  , desiredReplicationCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data FileMetadata = FileMetadata
  { name :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Files = MkFiles
  { count :: Int
  , rows  :: [File]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- Ipfs types to delegate backend

data TokenStatus = Minted | Burned
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TokenKey = MkTokenKey { tokenKey :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data MetaOptions = MkMetaOptions
  { status :: TokenStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = MkMetadata
  { name      :: Maybe Text
  , keyvalues :: Maybe MetaOptions
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Token = Token
  { pinataContent  :: TokenKey
  , pinataMetadata :: Metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

tokenSample :: Token
tokenSample = Token
  { pinataContent = MkTokenKey "super secret key"
  , pinataMetadata = MkMetadata
      { name = Just "tokenName"
      , keyvalues = Just $ MkMetaOptions Minted
      }
  }