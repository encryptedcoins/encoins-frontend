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
encoinsCurrencySymbol = "1940e97fcd60b84e8ffe88c966a4b85393727edcf8b9c4d55c55398d"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "b6c6632b5ecf39df368c9b6ac7ce201c565c7dd06a30b4a34db45d90")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")