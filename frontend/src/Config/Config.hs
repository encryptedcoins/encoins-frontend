{-# LANGUAGE CPP #-}

module Config.Config where

import           Data.FileEmbed               (embedFile)
import           Data.ByteString              (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T


urlsBS :: ByteString
urlsBS = $(embedFile "config/backend_url.json")

-- networkConfigBS :: ByteString
-- networkConfigBS = $(embedFile "config/network_id_config.json")

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
#ifdef PRE_DAO
daoNetwork = Testnet
#else
daoNetwork = Mainnet
#endif

appNetwork :: NetworkId
#ifdef PRE_APP
appNetwork = Testnet
#else
appNetwork = Mainnet
#endif

networkConfig :: NetworkConfig
networkConfig = NetworkConfig
  { dao = daoNetwork
  , app = appNetwork
  }
