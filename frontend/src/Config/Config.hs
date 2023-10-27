module Config.Config where

import           Data.FileEmbed               (embedFile)
import           Data.ByteString              (ByteString)

urlsBS :: ByteString
urlsBS = $(embedFile "config/backend_url.json")

networkConfigBS :: ByteString
networkConfigBS = $(embedFile "config/network_id_config.json")

bulletproofSetupBS :: ByteString
bulletproofSetupBS = $(embedFile "config/bulletproof_setup.json")

delegatorListBS :: ByteString
delegatorListBS = $(embedFile "../result/ispo/calculator.json")

delegateRelayAmount :: ByteString
delegateRelayAmount = $(embedFile "config/delegate_relay_amount.json")