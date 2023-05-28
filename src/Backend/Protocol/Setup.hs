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
encoinsCurrencySymbol = "fdf247ce6920c585859cc20b69f90bed8a398da527906e53944e3d71"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "ea31cfb813df4e90c6fd70fc58833ea7676fc1f4e4407c37e19fafdc")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")