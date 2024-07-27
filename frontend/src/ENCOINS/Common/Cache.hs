module ENCOINS.Common.Cache where

import Backend.Protocol.Types (PasswordRaw (..))
import Common.Events
import Common.Utility (toJsonText)
import JS.Website (loadJSON, removeKey, saveJSON)

import Common.Reflex.Dom.Extra (elementResultJS)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, decode, decodeStrict)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Storage (getItem, setItem)
import GHCJS.DOM.Types (MonadDOM)
import GHCJS.DOM.Window (getLocalStorage)
import Reflex.Dom

-------------------------------------------------------------------------------
-- Constants for browser cache
-------------------------------------------------------------------------------

encoinsV3 :: Text
encoinsV3 = "encoins-v3"

-- Discontinued.
-- It is used in migrating process.
encoinsV2 :: Text
encoinsV2 = "encoins-with-name"

-- Discontinued.
-- It is used in migrating process.
encoinsV1 :: Text
encoinsV1 = "encoins"

currentWallet :: Text
currentWallet = "current-wallet"

aesKey :: Text
aesKey = "encoins-aes-key"

isCloudOn :: Text
isCloudOn = "encoins-save-on"

passwordStorageKey :: Text
passwordStorageKey = "password-hash"

-------------------------------------------------------------------------------
-- Cache functions
-------------------------------------------------------------------------------

loadAppDataE ::
    forall t m a b.
    (MonadWidget t m, FromJSON a, Show a, Show b) =>
    Maybe PasswordRaw
    -> Text -- cache key
    -> Text -- response id
    -> (a -> b)
    -> b
    -> m (Dynamic t b)
loadAppDataE mPass key resId f val = do
    e <- newEventWithDelay 0.1
    loadAppData mPass key resId e f val

loadAppData ::
    forall t m a b.
    (MonadWidget t m, FromJSON a, Show a, Show b) =>
    Maybe PasswordRaw
    -> Text -- cache key
    -> Text -- response id
    -> Event t ()
    -> (a -> b)
    -> b
    -> m (Dynamic t b)
loadAppData mPass key resId ev f val = do
    dmRes <- loadAppDataM mPass key resId ev
    let dRes = maybe val f <$> dmRes
    pure dRes

loadAppDataME ::
    forall t m a.
    (MonadWidget t m, FromJSON a, Show a) =>
    Maybe PasswordRaw
    -> Text -- cache key
    -> Text -- response id
    -> m (Dynamic t (Maybe a))
loadAppDataME mPass key resId = do
    e <- newEventWithDelay 0.1
    loadAppDataM mPass key resId e

loadAppDataM ::
    forall t m a.
    (MonadWidget t m, FromJSON a, Show a) =>
    Maybe PasswordRaw
    -> Text -- cache key
    -> Text -- response id
    -> Event t ()
    -> m (Dynamic t (Maybe a))
loadAppDataM mPass key resId ev = do
    let mPassT = (getPassRaw <$> mPass)
    performEvent_ (loadJSON key resId mPassT <$ ev)
    dRes <-
        elementResultJS resId ((decodeStrict :: ByteString -> Maybe a) . encodeUtf8)
    pure dRes

saveAppData_ ::
    (MonadWidget t m, ToJSON a) =>
    Maybe PasswordRaw
    -> Text
    -> Event t a
    -> m ()
saveAppData_ mPass key eVal = do
    void $ saveAppData mPass key eVal

saveAppData ::
    (MonadWidget t m, ToJSON a) =>
    Maybe PasswordRaw
    -> Text
    -> Event t a
    -> m (Event t ())
saveAppData mPass key eVal = do
    let eEncodedValue = toJsonText <$> eVal
    let mPassT = (getPassRaw <$> mPass)
    performEvent (saveJSON mPassT key <$> eEncodedValue)

removeCacheKey ::
    (MonadWidget t m) =>
    Event t Text
    -> m (Event t ())
removeCacheKey eKey = performEvent (removeKey <$> eKey)

loadJsonFromStorage :: (MonadDOM m, FromJSON a) => Text -> m (Maybe a)
loadJsonFromStorage elId = do
    lc <- currentWindowUnchecked >>= getLocalStorage
    (>>= decode . fromStrict . encodeUtf8) <$> getItem lc elId

saveJsonToStorage :: (MonadDOM m, ToJSON a) => Text -> a -> m ()
saveJsonToStorage elId val = do
    lc <- currentWindowUnchecked >>= getLocalStorage
    setItem lc elId . toJsonText $ val

loadTextFromStorage :: (MonadDOM m) => Text -> m (Maybe Text)
loadTextFromStorage key = do
    lc <- currentWindowUnchecked >>= getLocalStorage
    getItem lc key
