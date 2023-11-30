module ENCOINS.DAO.Widgets.DelegateWindow.RelayNames
  (
    relayNames
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Text                       (Text)

relayNames :: Map Text Text
relayNames = fromList
  [
    ("http://156.232.53.23:3000/", "ENCOINS Test Relay"),
    ("http://209.145.61.188:3000", "PetLoverStake"),
    ("http://167.71.10.20", "Waterboiler"),
    ("http://61.77.248.249", "KTOP SPO"),
    ("http://128.199.195.27", "Waffle"),
    ("http://136.243.152.100", "[CHIL] Chile Stake pool"),
    ("http://216.152.65.248", "$conrad"),
    ("http://146.190.63.130", "Peter Bui"),
    ("https://5.189.164.41", "Markus"),
    ("http://82.165.230.141", "OYSTR"),
    ("http://146.190.34.49", "Kodjo AKPONDEOU"),
    ("http://encoins-relay.cardanistas.io", "Cardanistas Stake Pool"),
    ("http://encoins.reservoir.network", "Tom - BURN pool"),
    ("http://144.91.80.26", "Toby Relayer")
  ]