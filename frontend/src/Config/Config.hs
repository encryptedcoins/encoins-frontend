{-# LANGUAGE CPP #-}

module Config.Config where

import           Data.FileEmbed               (embedFile)
import           Data.ByteString              (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.Reflex               (BaseUrl (..))

bulletproofSetupBS :: ByteString
bulletproofSetupBS = $(embedFile "config/bulletproof_setup.json")

delegatorListBS :: ByteString
delegatorListBS = $(embedFile "../result/ispo/calculator.json")

data NetworkId = Testnet | Mainnet
  deriving (Eq, Show, Enum)

toNetworkId :: Text -> NetworkId
toNetworkId = toEnum . read @Int . T.unpack

data NetworkConfig = NetworkConfig
  { dao :: NetworkId
  , app :: NetworkId
  }
  deriving (Eq, Show)

daoNetwork :: NetworkId
#ifdef PREDAO
daoNetwork = Testnet
#else
daoNetwork = Mainnet
#endif

appNetwork :: NetworkId
#ifdef PREAPP
appNetwork = Testnet
#else
appNetwork = Mainnet
#endif

networkConfig :: NetworkConfig
networkConfig = NetworkConfig
  { dao = daoNetwork
  , app = appNetwork
  }

delegateServerUrl :: BaseUrl
delegateServerUrl = case daoNetwork of
  Mainnet -> BasePath "https://l2y0u35vje.execute-api.eu-central-1.amazonaws.com"
  Testnet -> BasePath "http://localhost:3002/"

jwtToken :: Text
jwtToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySW5mb3JtYXRpb24iOnsiaWQiOiJjMmMzNTNjMC04YzIwLTQ0YmQtODc5NC1mY2YxZDczYjMxOWQiLCJlbWFpbCI6ImNvbnRpbmdlbnRhbEBnbWFpbC5jb20iLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwicGluX3BvbGljeSI6eyJyZWdpb25zIjpbeyJpZCI6IkZSQTEiLCJkZXNpcmVkUmVwbGljYXRpb25Db3VudCI6MX0seyJpZCI6Ik5ZQzEiLCJkZXNpcmVkUmVwbGljYXRpb25Db3VudCI6MX1dLCJ2ZXJzaW9uIjoxfSwibWZhX2VuYWJsZWQiOmZhbHNlLCJzdGF0dXMiOiJBQ1RJVkUifSwiYXV0aGVudGljYXRpb25UeXBlIjoic2NvcGVkS2V5Iiwic2NvcGVkS2V5S2V5IjoiNjlmZGNhZDY1ZjgzNzYzZTJlMzMiLCJzY29wZWRLZXlTZWNyZXQiOiJlNjU4Yjg3ZWQ2YzAzNzNjYzk0MGYzZDJmMmE1OWE5NjkzOTk4NjY4MTYxMzhkMWQ1ZWM3YjA2ZTI5NzE3MzdiIiwiaWF0IjoxNzAzNzY0Njg1fQ.ANQVAtuW0xAKVNXu57zktHMF2_TwbAxi4ciIceLc2so"