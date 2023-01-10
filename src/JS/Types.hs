{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module JS.Types where

import           Data.Aeson                  (ToJSON(..), FromJSON (..))
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, pack)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (ToJSVal(..))
import           PlutusTx.Builtins
import           Text.Hex                    (decodeHex)

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