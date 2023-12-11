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
    ("http://encoins.bladepool.com", "BLADE"),
    ("http://146.190.63.130", "Peter Bui"),
    ("https://5.189.164.41", "SUDTI Pool"),
    ("http://82.165.230.141", "OYSTR"),
    ("http://146.190.34.49", "LABA-RELAY"),
    ("http://encoins-relay.cardanistas.io", "Cardanistas Stake Pool"),
    ("http://encoins.reservoir.network", "BURN pool"),
    ("http://213.136.92.202", "Toby Relay"),
    ("http://167.71.10.20", "zencs-relay"),
    ("http://38.242.237.17", "cardano.uno"),
    ("http://161.97.73.124", "SafeStride relay"),
    ("http://encs.chunkymonkey.us", "Chunky Monkey"),
    ("http://142.132.189.114", "KTOP SPO 2"),
    ("http://161.97.172.86", "PetLoverStake-II"),
    ("http://135.148.53.149", "Vortex Relay"),
    ("http://135.148.53.148", "Panda Relay"),
    ("http://125.250.255.197", "KTOP SPO 3"),
    ("http://encoins-maria-relay.cardanistas.io", "Maria Carmo 369"),
    ("http://95.179.167.197", "Maria Carmo 369"),
    ("http://2.59.156.182", "Toby 3 Relay"),
    ("http://81.56.175.1", "SUDTI Pool 3"),
    ("http://144.91.80.26", "Toby 2 Relay"),
    ("http://encoins02.bladepool.com", "BLADE02"),
    ("http://75.119.155.25", "SUDTI Pool 2"),
    ("http://117.50.199.89", "BAIDU"),
    ("http://172.205.248.34", "Waffle #2"),
    ("http://135.148.53.151", "Prism Relay"),
    ("http://161.97.88.36", "RUMOR"),
    ("http://136.244.19.126", "Cipher Relay"),
    ("http://23.105.171.80", "DC Private"),
    ("https://178.119.158.164", "KTOP SPO 4"),
    ("https://encoins.cardanode.com.au", "ADAOZ"),
    ("http://encoins.congeepool.com", "[CONGE] Conge Pool"),
    ("http://154.38.171.77", "Toby 4 Relay"),
    ("http://149.102.156.213", "Toby 5 Relay (Private)"),
    ("http://176.57.184.115", "Toby 6 Relay")
  ]