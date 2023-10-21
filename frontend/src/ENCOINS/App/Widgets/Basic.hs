module ENCOINS.App.Widgets.Basic where

import           Control.Monad            (when)
import           Data.Aeson               (FromJSON, ToJSON, decode,
                                           decodeStrict, encode)
import           Data.Bool                (bool)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (fromStrict, toStrict)
import           Data.Maybe               (isNothing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           GHCJS.DOM                (currentWindowUnchecked)
import           GHCJS.DOM.Storage        (getItem, setItem)
import           GHCJS.DOM.Types          (MonadDOM)
import           GHCJS.DOM.Window         (getLocalStorage)
import           Reflex.Dom
import           Reflex.ScriptDependent   (widgetHoldUntilDefined)
import           Servant.Reflex           (BaseUrl)


import           Backend.Servant.Requests (getRelayUrl)
import           Backend.Status           (Status (..), everyRelayDown)
import           ENCOINS.Common.Events    (newEvent, newEventWithDelay)
import           JS.Website               (loadJSON)

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

loadAppData :: forall t m a b . (MonadWidget t m, FromJSON a) =>
  Maybe Text -> Text -> (a -> b) -> b -> m (Dynamic t b)
loadAppData mpass entry f val = do
    let elId = "elId-" <> entry
    e <- newEventWithDelay 0.1
    performEvent_ (loadJSON mpass entry elId <$ e)
    elementResultJS elId (maybe val f . (decodeStrict :: ByteString -> Maybe a) . encodeUtf8)

loadJsonFromStorage :: (MonadDOM m, FromJSON a) => Text -> m (Maybe a)
loadJsonFromStorage elId = do
  lc <- currentWindowUnchecked >>= getLocalStorage
  (>>= decode . fromStrict . encodeUtf8) <$> getItem lc elId

saveJsonToStorage :: (MonadDOM m, ToJSON a) => Text -> a -> m ()
saveJsonToStorage elId val = do
  lc <- currentWindowUnchecked >>= getLocalStorage
  setItem lc elId . decodeUtf8 . toStrict . encode $ val

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

relayStatus :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m) => m ()
relayStatus = do
  relayStatusM =<< getRelayUrl

relayStatusM :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe BaseUrl
  -> m ()
relayStatusM mRelayUrl = do
  when (isNothing mRelayUrl) $ do
    ev <- newEvent
    tellRelayStatus
      "Relay status"
      (BackendError everyRelayDown)
      ev

tellRelayStatus :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Text
  -> Status
  -> Event t ()
  -> m ()
tellRelayStatus title status ev = tellEvent $ [(title, status) <$ ev] <$ ev

tellTxStatus :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Text
  -> Status
  -> Event t Status
  -> m ()
tellTxStatus title status ev =
  tellEvent $
      [(\x -> bool (title, x) (T.empty, status) $ x == status) <$> ev] <$ ev
