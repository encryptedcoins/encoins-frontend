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
minAdaTxOutInLedger = 2_000_000

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "f521b2598cd04125d3762078685d50b48d680bd31455e6711671595e"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "cd50deb19731537b7cd5dc3b99e54a8561bc898e5ad5135e35fc6414")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")