{-# LANGUAGE NumericUnderscores #-}

module Backend.Protocol.Setup where

import           Data.Aeson             (decode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           PlutusTx.Builtins
import           Text.Hex               (decodeHex)

import           Backend.Protocol.Types
import           Config.Config          (NetworkConfig (..), NetworkId (..),
                                         bulletproofSetupBS, networkConfig)
import           ENCOINS.Bulletproofs

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict bulletproofSetupBS

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 4_000_000

encoinsCurrencySymbol :: Text
encoinsCurrencySymbol = case app networkConfig of
  Mainnet -> "f8fb0501e8966286e9e188860c4a48ee856eff5ae23d988812b3fcc2"
  Testnet -> "fafd6f6d1a319af7f4a9638345ce7c3992dc3ba42bf6c99008e4ecb3"

ledgerAddress :: Address
ledgerAddress = Address
  (ScriptCredential
    $ ValidatorHash
    $ toBuiltin
    $ fromJust
    $ decodeHex validatorHex)
  (Just
    $ StakingHash
    $ PubKeyCredential
    $ PubKeyHash
    $ toBuiltin
    $ fromJust
    $ decodeHex pubKeyHex)
  where
    validatorHex = case app networkConfig of
      Mainnet -> "93ee21257ac2a126e3afb273cbf59635365d7f554751ab936bcaddf0"
      Testnet -> "4eca152b92576fcd88c4621f194a356273236f957fdb287cfd0ee4a3"
    pubKeyHex = case app networkConfig of
      Mainnet -> "0bd016f8ba5857d2e2026da550e4b724a3e1e8d5598cbfab19ce756c"
      Testnet -> "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390"
