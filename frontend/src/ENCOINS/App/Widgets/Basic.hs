module ENCOINS.App.Widgets.Basic where

import           Data.Aeson             (FromJSON, ToJSON, decode, decodeStrict,
                                         encode)
import           Data.Bool              (bool)
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           GHCJS.DOM              (currentWindowUnchecked)
import           GHCJS.DOM.Storage      (getItem, setItem)
import           GHCJS.DOM.Types        (MonadDOM)
import           GHCJS.DOM.Window       (getLocalStorage)
import           Reflex.Dom
import           Reflex.ScriptDependent (widgetHoldUntilDefined)


import           Backend.Status         (Status (..))
import           ENCOINS.Common.Events
import           JS.Website             (loadJSON)

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

loadAppData :: forall t m a b . (MonadWidget t m, FromJSON a)
  => Maybe Text
  -> Text
  -> (a -> b)
  -> b
  -> m (Dynamic t b)
loadAppData mpass entry f val = do
    let elId = "elId-" <> entry
    e <- newEventWithDelay 0.1
    performEvent_ (loadJSON mpass entry elId <$ e)
    elementResultJS elId (maybe val f . (decodeStrict :: ByteString -> Maybe a) . encodeUtf8)

loadAppDataId :: forall t m a b . (MonadWidget t m, FromJSON a)
  => Maybe Text
  -> Text
  -> Text
  -> Event t ()
  -> (a -> b)
  -> b
  -> m (Dynamic t b)
loadAppDataId mpass entry elId ev f val = do
    eDelayed <- delay 0.1 ev
    performEvent_ (loadJSON mpass entry elId <$ eDelayed)
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

tellTxStatus :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Text -- Category of the status. E.g. 'wallet mode'
  -> Event t Status
  -> m ()
tellTxStatus title ev =
  tellEvent $
      [(\x -> bool (title, x) (T.empty, Ready) $ x == Ready) <$> ev] <$ ev
