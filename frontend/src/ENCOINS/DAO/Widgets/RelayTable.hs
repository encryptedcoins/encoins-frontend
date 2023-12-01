{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RecursiveDo    #-}

module ENCOINS.DAO.Widgets.RelayTable
  (
    relayAmountWidget
  , fetchRelayTable
  , fetchRelayTableFile
  ) where

import           Control.Monad                (forM)
import           Data.List                    (sortOn)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.Ord                     (Down (..))
import           Data.Text                    (Text)
import           Numeric.Natural              (Natural)
import           Reflex.Dom

import           Backend.Servant.Requests     (serversRequestWrapper)
import           Backend.Utility              (switchHoldDyn)
import           Config.Config                (delegateServerUrl)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils         (toText)
import           ENCOINS.Common.Widgets.Basic (btn)
import           ENCOINS.DAO.Widgets.DelegateWindow.RelayNames (relayNames)

relayAmountWidget :: MonadWidget t m
  => Dynamic t [(Text, Integer)]
  -> m (Event t Text)
relayAmountWidget dRelays = do
  article $ tableWrapper $
    table $ do
      el "thead" $ tr $
        mapM_ (\h -> th $ text h) ["Relay", "Delegated", ""]
      el "tbody" $ do
        switchHoldDyn dRelays $ \relays -> do
          evs <- forM relays $ \(relay, amount) -> tr $ do
            tdRelay $ text $ fromMaybe relay (relay `Map.lookup` relayNames)
            tdAmount
              $ text
              $ toText (floor @Double $ fromIntegral amount / 1000000 :: Integer) <> " ENCS"
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

fetchRelayTableFile :: MonadWidget t m
  => Event t ()
  -> m (Event t [(Text, Integer)])
fetchRelayTableFile eOpen = do
  let eUrl = "https://encoins.io/delegations.json" <$ eOpen
  fmap sortRelayAmounts <$> getAndDecode eUrl

fetchRelayTable :: MonadWidget t m
  => Event t ()
  -> m (Event t [(Text, Integer)])
fetchRelayTable eOpen = do
  eServers <- serversRequestWrapper delegateServerUrl eOpen
  -- TODO: handle error in interface?
  let eError = filterLeft eServers
  logEvent "Fetching relay table failed" eError
  let res = sortOn (Down . snd) . Map.toList <$> filterRight eServers
  pure res
