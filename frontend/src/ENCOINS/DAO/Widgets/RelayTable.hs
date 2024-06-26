{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Widgets.RelayTable
    ( fetchDelegatedByAddress
    , fetchRelayNames
    , fetchRelayTable
    , relayAmountWidget
    , unStakeUrl
    ) where

import Control.Monad (forM)
import Data.Bool (bool)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Reflex.Dom

import Backend.Protocol.Types
import Backend.Servant.Requests (infoRequestWrapper, serversRequestWrapper)
import Backend.Utility (switchHoldDyn, toText)
import Config.Config (delegateServerUrl)
import ENCOINS.Common.Events
import ENCOINS.Common.Utils (stripHostOrRelay)
import ENCOINS.Common.Widgets.Basic (btnWithBlock)

relayAmountWidget ::
    (MonadWidget t m) =>
    Event t (Either Text [(Text, Integer)])
    -> Event t (Maybe (Text, Integer))
    -> Dynamic t (Map Text Text)
    -> m (Event t Text)
relayAmountWidget eeRelays emDelegated dRelayNames = do
    deRelays <- holdDyn (Right []) eeRelays
    dmDelegated <- holdDyn Nothing emDelegated
    switchHoldDyn deRelays $ \case
        Left err -> do
            article $
                divClass "" $
                    text $
                        "Fetching delegate relays has failed with error: " <> err
            pure never
        Right relays -> article $ table $ do
            let stripedRelays = map (\(u, n) -> (stripHostOrRelay u, n)) relays
            el "thead" $
                tr $
                    mapM_ (\h -> th $ text h) ["Relay", "Total", ""]
            el "tbody" $ do
                evs <- forM stripedRelays $ \(relay, amount) ->
                    if unStakeUrl == relay
                        then do
                            blank
                            pure never
                        else tr $ do
                            tdRelay $ dynText $ fromMaybe relay . Map.lookup relay <$> dRelayNames
                            tdAmount $ text $ mkAmount amount
                            eClick <-
                                tdButton $
                                    btnWithBlock
                                        "button-switching inverted"
                                        ""
                                        (isDelegated relay <$> dmDelegated)
                                        (dynText $ mkDelegateButton relay <$> dmDelegated)
                            pure $ relay <$ eClick
                pure $ leftmost evs
    where
        article = elAttr "article" ("class" =: "dao-DelegateWindow_TableWrapper")
        table = elAttr "table" ("class" =: "dao-DelegateWindow_Table")
        tr = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow")
        th = elAttr "th" ("class" =: "dao-DelegateWindow_TableHeader")
        tdRelay = elAttr "td" ("class" =: "dao-DelegateWindow_TableRelay")
        tdAmount = elAttr "td" ("class" =: "dao-DelegateWindow_TableAmount")
        tdButton = elAttr "td" ("class" =: "dao-DelegateWindow_TableButton")

fetchRelayTable ::
    (MonadWidget t m) =>
    Event t ()
    -> m (Event t (Either Text [(Text, Integer)]))
fetchRelayTable eOpen = do
    eServers <- serversRequestWrapper delegateServerUrl eOpen
    let eeRes = fmap (sortOn (Down . snd) . Map.toList) <$> eServers
    pure eeRes

fetchDelegatedByAddress ::
    (MonadWidget t m) =>
    Dynamic t Address
    -> Event t ()
    -> m (Event t (Maybe (Text, Integer)))
fetchDelegatedByAddress dAddr eFire = do
    eeInfo <- infoRequestWrapper delegateServerUrl dAddr eFire
    let meInfo = either (const Nothing) Just <$> eeInfo
    pure meInfo

mkAmount :: Integer -> Text
mkAmount amount =
    toText (floor @Double $ fromIntegral amount / 1000000 :: Integer) <> " ENCS"

mkDelegateButton :: Text -> Maybe (Text, Integer) -> Text
mkDelegateButton relay =
    maybe "Delegate" (\(r, n) -> bool "Delegate" (mkAmount n) (r == relay))

isDelegated :: Text -> Maybe (Text, Integer) -> Bool
isDelegated relay = \case
    Nothing -> False
    Just (r, _) -> r == relay

unStakeUrl :: Text
unStakeUrl = "encoins.io"

-- sortRelayAmounts :: Maybe (Map Text String) -> [(Text, Integer)]
-- sortRelayAmounts =
--     sortOn (Down . snd)
--   . Map.toList
--   . Map.map (floor @Double . (\x -> fromIntegral x / 1000000) . read @Natural )
--   . fromJust

-- fetchRelayTable :: MonadWidget t m
--   => Event t ()
--   -> m (Event t [(Text, Integer)])
-- fetchRelayTable eOpen = do
--   let eUrl = "https://encoins.io/delegations.json" <$ eOpen
--   fmap sortRelayAmounts <$> getAndDecode eUrl

fetchRelayNames ::
    (MonadWidget t m) =>
    Event t ()
    -> m (Dynamic t (Map Text Text))
fetchRelayNames eOpen = do
    let eUrl = "https://encoins.io/relay_names.json" <$ eOpen
    (emNames :: Event t (Maybe (Map Text Text))) <- getAndDecode eUrl
    dmNames <- holdDyn Nothing emNames
    eNames <- switchHoldDyn dmNames $ \case
        Nothing -> do
            logEvent "Nothing fetched from url " eUrl
            pure never
        Just names -> do
            e <- newEvent
            pure $ names <$ e
    holdDyn Map.empty eNames
