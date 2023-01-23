{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

module Backend.Types where

import           Data.Aeson                  (ToJSON(..), FromJSON (..))
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           PlutusTx.Builtins
import           Text.Hex                    (decodeHex)

import           ENCOINS.BaseTypes           (MintingPolarity)
import           ENCOINS.Bulletproofs        (Proof (..))

type TxParams = Address

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
    deriving (ToJSON, FromJSON)
newtype ValidatorHash = ValidatorHash BuiltinByteString
    deriving stock (Generic, Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

mkAddressFromPubKeys :: Text -> Text -> Address
mkAddressFromPubKeys pkhHex skhHex = Address (PubKeyCredential $ PubKeyHash pkh) (Just $ StakingHash $ PubKeyCredential $ PubKeyHash skh)
    where pkh = toBuiltin $ fromJust $ decodeHex pkhHex
          skh = toBuiltin $ fromJust $ decodeHex skhHex

type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)
type EncoinsRedeemerWithData = (Address, EncoinsRedeemer)

newtype PubKey = PubKey { getPubKey :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

newtype Signature = Signature { getSignature :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)