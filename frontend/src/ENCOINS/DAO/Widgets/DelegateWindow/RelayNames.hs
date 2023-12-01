module ENCOINS.DAO.Widgets.DelegateWindow.RelayNames
  (
    relayNames
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Text                       (Text)

relayNames :: Map Text Text
relayNames = fromList
  [
    ("http://73.23.36.140", "PetLoverStake"),
    ("http://available", ""),
    ("http://61.77.248.249", "KTOP SPO"),
    ("http://128.199.195.27", "Waffle"),
    ("https://136.243.152.100", "[CHIL] Chile Stake pool"),
    ("http://encoins.bladepool.com", "$conrad"),
    ("http://146.190.63.130", "Peter Bui"),
    ("https://5.189.164.41", "Southtyrol Pool"),
    ("http://82.165.230.141", "OYSTR"),
    ("http://146.190.34.49", "Kodjo AKPONDEOU"),
    ("http://encoins-relay.cardanistas.io", "Cardanistas Stake Pool"),
    ("http://encoins.reservoir.network", "BURN pool"),
    ("http://144.91.80.26", "Toby Relayer"),
    ("http://167.71.10.20", "zencs-relay"),
    ("http://38.242.237.17", "cardano.uno"),
    ("http://161.97.73.124", "SafeStride relay")
  ]