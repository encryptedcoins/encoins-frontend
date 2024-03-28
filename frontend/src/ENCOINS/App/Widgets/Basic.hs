module ENCOINS.App.Widgets.Basic where

import           Backend.Protocol.Types (PasswordRaw (..))
import           Backend.Status         (AppStatus (..), CloudStatusIcon (..),
                                         Status (..))
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils   (toJsonText)
import           JS.Website             (loadJSON, saveJSON, removeKey)


import           Control.Monad          (void)
import           Data.Aeson             (FromJSON, ToJSON, decode, decodeStrict)
import           Data.Bool              (bool)
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           GHCJS.DOM              (currentWindowUnchecked)
import           GHCJS.DOM.Storage      (getItem, setItem)
import           GHCJS.DOM.Types        (MonadDOM)
import           GHCJS.DOM.Window       (getLocalStorage)
import           Reflex.Dom
import           Reflex.ScriptDependent (widgetHoldUntilDefined)


sectionApp :: MonadWidget t m => Text -> Text -> m a -> m a
sectionApp elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section-app wf-section " `T.append` cls)

containerApp :: MonadWidget t m => Text -> m a -> m a
containerApp cls = divClass ("container-app w-container " `T.append` cls)

-- Element containing the result of a JavaScript computation
elementResultJS :: MonadWidget t m => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f = fmap (fmap f . value) $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId

waitForScripts :: MonadWidget t m => m () -> m () -> m ()
waitForScripts placeholderWidget actualWidget = do
  ePB <- getPostBuild
  _ <- widgetHoldUntilDefined "walletAPI" ("js/ENCOINS.js" <$ ePB) placeholderWidget actualWidget
  blank

loadAppDataE :: forall t m a b . (MonadWidget t m, FromJSON a, Show a, Show b)
  => Maybe PasswordRaw
  -> Text -- cache key
  -> Text -- response id
  -> (a -> b)
  -> b
  -> m (Dynamic t b)
loadAppDataE mPass key resId f val = do
    e <- newEventWithDelay 0.1
    loadAppData mPass key resId e f val

loadAppData :: forall t m a b . (MonadWidget t m, FromJSON a, Show a, Show b)
  => Maybe PasswordRaw
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

loadAppDataME :: forall t m a . (MonadWidget t m, FromJSON a, Show a)
  => Maybe PasswordRaw
  -> Text -- cache key
  -> Text -- response id
  -> m (Dynamic t (Maybe a))
loadAppDataME mPass key resId = do
    e <- newEventWithDelay 0.1
    loadAppDataM mPass key resId e

loadAppDataM :: forall t m a . (MonadWidget t m, FromJSON a, Show a)
  => Maybe PasswordRaw
  -> Text -- cache key
  -> Text -- response id
  -> Event t ()
  -> m (Dynamic t (Maybe a))
loadAppDataM mPass key resId ev = do
    let mPassT = (getPassRaw <$> mPass)
    performEvent_ (loadJSON key resId mPassT <$ ev)
    dRes <- elementResultJS resId ((decodeStrict :: ByteString -> Maybe a) . encodeUtf8)
    pure dRes

saveAppData_ :: (MonadWidget t m, ToJSON a)
  => Maybe PasswordRaw
  -> Text
  -> Event t a
  -> m ()
saveAppData_ mPass key eVal = do
    void $ saveAppData mPass key eVal

saveAppData :: (MonadWidget t m, ToJSON a)
  => Maybe PasswordRaw
  -> Text
  -> Event t a
  -> m (Event t ())
saveAppData mPass key eVal = do
    let eEncodedValue = toJsonText <$> eVal
    let mPassT = (getPassRaw <$> mPass)
    performEvent (saveJSON mPassT key <$> eEncodedValue)

removeCacheKey :: MonadWidget t m
  => Event t Text
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

loadTextFromStorage :: MonadDOM m => Text -> m (Maybe Text)
loadTextFromStorage key = do
  lc <- currentWindowUnchecked >>= getLocalStorage
  getItem lc key

-- Wallet error element
walletError :: MonadWidget t m => m (Event t Status)
walletError = do
    dWalletError <- elementResultJS "walletErrorElement" id
    let eWalletError = ffilter ("" /=) $ updated dWalletError
    return $ WalletError <$> eWalletError

tellTxStatus :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Text -- Category of the status. E.g. 'wallet mode'
  -> Event t Status
  -> m ()
tellTxStatus title ev =
  tellEvent $
      (\x -> singletonL . Tx . bool (title, x) (T.empty, Ready) $ x == Ready) <$> ev

tellSaveStatus :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Event t CloudStatusIcon
  -> m ()
tellSaveStatus ev = tellEvent $ (singletonL . Save) <$> ev

-- it added to base from 4.15.0.0
-- we are on base-4.12.0.0
singletonL :: a -> [a]
singletonL x = [x]