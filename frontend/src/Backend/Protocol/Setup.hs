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
encoinsCurrencySymbol = "f8fb0501e8966286e9e188860c4a48ee856eff5ae23d988812b3fcc2"

ledgerAddress :: Address
ledgerAddress = Address (ScriptCredential $ ValidatorHash $ toBuiltin $ fromJust $
    decodeHex "93ee21257ac2a126e3afb273cbf59635365d7f554751ab936bcaddf0")
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin $ fromJust $
    decodeHex "0bd016f8ba5857d2e2026da550e4b724a3e1e8d5598cbfab19ce756c")