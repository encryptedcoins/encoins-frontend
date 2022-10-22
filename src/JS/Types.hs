{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.Types where

import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (ToJSVal(..))

-- Types that are passed to our JS functions as arguments

newtype EncoinsRedeemer = EncoinsRedeemer ([(Text, Text)], Text)
    deriving (Eq, Show, Generic, ToJSVal)