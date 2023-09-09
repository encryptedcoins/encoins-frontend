{-# LANGUAGE NumericUnderscores #-}

module Backend.Protocol.Setup where

import           Data.Aeson                      (decode)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.FileEmbed                  (embedFile)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import           PlutusTx.Builtins
import           Text.Hex                        (decodeHex)

import           Backend.Protocol.Types
import           ENCOINS.Bulletproofs

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 4_000_000

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "3365fb88a2777bb436f4d29c495f3c7ce1c7c0493f3925dd087d9704"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "2dd2954c20a20ba371cf7bc8efe878579067b2f10da142c6d27df6c2")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")