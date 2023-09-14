{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

module Backend.Protocol.Types where

import           Data.Aeson                  (ToJSON(..), FromJSON (..))
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text as Text
import           GHC.Generics                (Generic)
import           PlutusTx.Builtins
import           Reflex.Dom                  (decodeText)
import           Text.Hex                    (decodeHex, encodeHex)

import           CSL                         (TransactionUnspentOutputs)
import           ENCOINS.BaseTypes           (MintingPolarity)
import           ENCOINS.Bulletproofs        (Proof)

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
          PubKeyCredential (PubKeyHash pkh) -> pkh
          ScriptCredential (ValidatorHash vh) -> vh
        bs2 = encodeHex $ fromBuiltin $ case scr of
          Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> pkh
          Just (StakingHash (ScriptCredential (ValidatorHash vh))) -> vh
          _       -> emptyByteString

checkEmptyText :: Text -> Maybe Text
checkEmptyText "" = Nothing
checkEmptyText txt = Just txt

type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

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
        submitReqTx         :: Text,
        submitReqWitnesses  :: [(Text, Text)]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data EncoinsMode = WalletMode | LedgerMode
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data EncoinsStatusReqBody = MaxAdaWithdraw | LedgerEncoins
    deriving (Show, Eq, Enum, Generic, FromJSON, ToJSON)

data EncoinsStatusResult = MaxAdaWithdrawResult Integer | LedgerUtxoResult TransactionUnspentOutputs
    deriving (Show, Eq, Generic, FromJSON, ToJSON)