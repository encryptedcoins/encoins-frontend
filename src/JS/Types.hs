{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module JS.Types where

import           Data.Text                   (Text, pack)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (ToJSVal(..))
import           PlutusTx.Builtins

import           ENCOINS.BaseTypes           (MintingPolarity, GroupElement (..))
import           ENCOINS.Bulletproofs        (Proof (..))
import           ENCOINS.Crypto.Field        (Field(..))

-- Types that are passed to our JS functions as arguments
type Address = Text

type TxParams = Address
type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

newtype EncoinsRedeemerJS = EncoinsRedeemerJS EncoinsRedeemer
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