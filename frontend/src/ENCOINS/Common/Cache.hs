module ENCOINS.Common.Cache where

import           Data.Text (Text)

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

ipfsCacheKey :: Text
ipfsCacheKey = "encoins-aes-key"

isIpfsOn :: Text
isIpfsOn = "encoins-ipfs-on"