{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RecursiveDo    #-}

module ENCOINS.DAO.Widgets.RelayTable
  (
    relayAmountWidget
  , fetchRelayTable
  ) where

import           Control.Monad                (forM)
import           Data.List                    (sortOn)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Data.Ord                     (Down (..))
import           Data.Text                    (Text)
import           Numeric.Natural              (Natural)
import           Reflex.Dom

import           Backend.Utility              (switchHoldDyn)
import           ENCOINS.Common.Utils         (toText)
import           ENCOINS.Common.Widgets.Basic (btn)

relayAmountWidget :: MonadWidget t m
  => Dynamic t [(Text, Integer)]
  -> m (Event t Text)
relayAmountWidget dRelays = do
  article $ tableWrapper $
    table $ do
      el "thead" $ tr $
        mapM_ (\h -> th $ text h) ["Relay", "Amount", ""]
      el "tbody" $ do
        switchHoldDyn dRelays $ \relays -> do
          evs <- forM relays $ \(relay, amount) -> tr $ do
            tdRelay $ text relay
            tdAmount $ text $ toText amount
            eClick <- tdButton $ btn "button-switching inverted" "" $ text "Delegate"
            pure $ relay <$ eClick
          pure $ leftmost evs
  where
    tableWrapper = elAttr "div" ("class" =: "dao-DelegateWindow_TableWrapper")
    table = elAttr "table" ("class" =: "dao-DelegateWindow_Table")
    article = elAttr "article" ("class" =: "dao-DelegateWindow_RelayAmount")
    tr = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow")
    th = elAttr "th" ("class" =: "dao-DelegateWindow_TableHeader")
    tdRelay = elAttr "td" ("class" =: "dao-DelegateWindow_TableRelay")
    tdAmount = elAttr "td" ("class" =: "dao-DelegateWindow_TableAmount")
    tdButton = elAttr "td" ("class" =: "dao-DelegateWindow_TableButton")

sortRelayAmounts :: Maybe (Map Text String) -> [(Text, Integer)]
sortRelayAmounts =
    sortOn (Down . snd)
  . Map.toList
  . Map.map (floor @Double . (\x -> fromIntegral x / 1000000) . read @Natural )
  . fromJust

fetchRelayTable :: MonadWidget t m
  => Event t ()
  -> m (Event t [(Text, Integer)])
fetchRelayTable eOpen = do
  let eUrl = "https://encoins.io/delegations.json" <$ eOpen
  fmap sortRelayAmounts <$> getAndDecode eUrl
