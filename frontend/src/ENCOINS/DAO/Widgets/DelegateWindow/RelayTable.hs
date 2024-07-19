{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Widgets.DelegateWindow.RelayTable
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
                        else do
                            let normalAmount = normalizeAmount amount
                            let dRelayName = fromMaybe relay . Map.lookup relay <$> dRelayNames
                            let dDelegateBlock = isDelegated relay <$> dmDelegated
                            let dDelegateTag = dynText $ mkDelegateButton relay <$> dmDelegated
                            ev <- makeDelegateRow 
                                normalAmount 
                                dRelayName 
                                dDelegateBlock 
                                dDelegateTag 
                            pure $ relay <$ ev
                pure $ leftmost evs
    where
        article = elAttr "article" ("class" =: "dao-DelegateWindow_TableWrapper")
        table = elAttr "table" ("class" =: "dao-DelegateWindow_Table")
        th = elAttr "th" ("class" =: "dao-DelegateWindow_TableHeader")


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

normalizeAmount :: Integer -> Integer
normalizeAmount amount = floor @Double $ fromIntegral amount / 1000000 :: Integer

mkAmount :: Integer -> Text
mkAmount amount =
    toText amount <> " ENCS"

mkDelegateButton :: Text -> Maybe (Text, Integer) -> Text
mkDelegateButton relay =
    maybe "Delegate" (\(r, n) -> bool "Delegate" (mkAmount $ normalizeAmount n) (r == relay))

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

makeDelegateRow :: 
    (MonadWidget t m) =>
    Integer
    -> Dynamic t Text 
    -> Dynamic t Bool
    -> m ()
    -> m (Event t ()) 
makeDelegateRow normalAmount dRelayName dDelegateBlock dDelegateTag = 
    rainbowTr normalAmount $ do
        tdRelay $ dynText dRelayName
        tdAmount $ text $ mkAmount normalAmount
        eClick <-
            tdButton $
                btnWithBlock
                    "button-switching inverted"
                    ""
                    dDelegateBlock
                    dDelegateTag
        pure eClick
    where
        trRed = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow-red")
        trYellow = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow-yellow")
        trGreen = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow-green")
        tdRelay = elAttr "td" ("class" =: "dao-DelegateWindow_TableRelay")
        tdAmount = elAttr "td" ("class" =: "dao-DelegateWindow_TableAmount")
        tdButton = elAttr "td" ("class" =: "dao-DelegateWindow_TableButton")
        rainbowTr stakedAmount
            | stakedAmount > 100000 = trRed
            | stakedAmount <= 100000 && stakedAmount > 90000 = trYellow
            | stakedAmount <= 90000 && stakedAmount > 50000 = trGreen
            | otherwise = tr
    
tr :: (DomBuilder t m) => m a -> m a
tr = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow")
