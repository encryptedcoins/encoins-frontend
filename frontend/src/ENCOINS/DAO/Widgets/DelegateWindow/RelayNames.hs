module ENCOINS.DAO.Widgets.DelegateWindow.RelayNames
  (
    relayNames
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Text                       (Text)

relayNames :: Map Text Text
relayNames = fromList
  [
    ("http://156.232.53.23:3000/", "ENCOINS Test Relay")
  ]