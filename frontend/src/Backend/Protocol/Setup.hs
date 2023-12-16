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
  Testnet -> "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"

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
      Testnet -> "09cc78edf1b8cd8243613de8d3ee6d6c8752e60241fe02929b5e8685"
    pubKeyHex = case app networkConfig of
      Mainnet -> "0bd016f8ba5857d2e2026da550e4b724a3e1e8d5598cbfab19ce756c"
      Testnet -> "0bd016f8ba5857d2e2026da550e4b724a3e1e8d5598cbfab19ce756c"

emergentChangeAddress :: Address
emergentChangeAddress = Address
  (PubKeyCredential
    $ PubKeyHash
    $ toBuiltin
    $ fromJust
    $ decodeHex pubKeyHex)
  (Just
    $ StakingHash
    $ PubKeyCredential
    $ PubKeyHash
    $ toBuiltin
    $ fromJust
    $ decodeHex stakeKeyHex)
  where
    pubKeyHex = case app networkConfig of
      Mainnet -> "8037261546e47ad3d628dcf2c17241e0c2dd641b323d1f8c466eaa50"
      Testnet -> "cfc1a6d381d58c108b56127c75262dd6201a69c3fc5a51b4e194bebb"
    stakeKeyHex = case app networkConfig of
      Mainnet -> "c1d7e99321128f9de0d21cd98b17de09056c1274cdd5c1af55ef622c"
      Testnet -> "c1d7e99321128f9de0d21cd98b17de09056c1274cdd5c1af55ef622c"