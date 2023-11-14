{-# LANGUAGE NumericUnderscores #-}

module Backend.Protocol.Setup where

import           Data.Aeson             (decode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           PlutusTx.Builtins
import           Text.Hex               (decodeHex)

import           Backend.Protocol.Types
import           Config.Config          (bulletproofSetupBS)
import           ENCOINS.Bulletproofs

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict bulletproofSetupBS

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 4_000_000

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = "fafd6f6d1a319af7f4a9638345ce7c3992dc3ba42bf6c99008e4ecb3"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "4eca152b92576fcd88c4621f194a356273236f957fdb287cfd0ee4a3")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")
