module ENCOINS.Common.Cache where

import Data.Text (Text)

encoinsV3 :: Text
encoinsV3 = "encoins-v3"

-- Discontinued.
-- It is used in migrating process.
encoinsV2 :: Text
encoinsV2 = "encoins-with-name"

-- Discontinued.
-- It is used in migrating process.
encoinsV1 :: Text
encoinsV1 = "encoins"

currentWallet :: Text
currentWallet = "current-wallet"

aesKey :: Text
aesKey = "encoins-aes-key"

isCloudOn :: Text
isCloudOn = "encoins-save-on"

passwordStorageKey :: Text
passwordStorageKey = "password-hash"
