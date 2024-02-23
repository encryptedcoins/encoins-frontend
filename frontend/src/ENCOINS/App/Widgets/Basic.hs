module ENCOINS.App.Widgets.Basic where

import           Backend.Protocol.Types (PasswordRaw (..))
import           Backend.Status         (AppStatus (..), IpfsSaveStatus (..),
                                         Status (..))
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils   (toJsonText)
import           JS.Website             (loadJSON, saveJSON)


import           Control.Monad          (join, void)
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

elementDynResultJS :: MonadWidget t m
  => (Text -> a)
  -> Dynamic t Text
  -> m (Dynamic t a)
elementDynResultJS f dResId = do
  let d resId = def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId
  let dmElement = (\resId -> inputElement $ d resId) <$> dResId
  let dmdVal = fmap value <$> dmElement
  edVal <- dyn dmdVal
  ddVal <- holdDyn (constDyn "") edVal
  pure $ f <$> join ddVal

waitForScripts :: MonadWidget t m => m () -> m () -> m ()
waitForScripts placeholderWidget actualWidget = do
  ePB <- getPostBuild
  _ <- widgetHoldUntilDefined "walletAPI" ("js/ENCOINS.js" <$ ePB) placeholderWidget actualWidget
  blank

loadAppData :: forall t m a b . (MonadWidget t m, FromJSON a, Show b)
  => Maybe PasswordRaw
  -> Text -- cache key
  -> Text -- response id
  -> (a -> b)
  -> b
  -> m (Dynamic t b)
loadAppData mPass key resId f val = do
    e <- newEventWithDelay 0.1
    loadAppDataId mPass key resId e f val

loadAppDataId :: forall t m a b . (MonadWidget t m, FromJSON a, Show b)
  => Maybe PasswordRaw
  -> Text -- cache key
  -> Text -- response id
  -> Event t ()
  -> (a -> b)
  -> b
  -> m (Dynamic t b)
loadAppDataId mPass key resId ev f val = do
    let mPassT = (getPassRaw <$> mPass)
    performEvent_ (loadJSON key resId mPassT <$ ev)
    dRes <- elementResultJS resId
      (maybe val f . (decodeStrict :: ByteString -> Maybe a) . encodeUtf8)
    pure dRes

saveAppDataId_ :: (MonadWidget t m, ToJSON a)
  => Maybe PasswordRaw
  -> Text
  -> Event t a
  -> m ()
saveAppDataId_ mPass key eVal = do
    void $ saveAppDataId mPass key eVal

saveAppDataId :: (MonadWidget t m, ToJSON a)
  => Maybe PasswordRaw
  -> Text
  -> Event t a
  -> m (Event t ())
saveAppDataId mPass key eVal = do
    let eEncodedValue = toJsonText <$> eVal
    let mPassT = (getPassRaw <$> mPass)
    performEvent (saveJSON mPassT key <$> eEncodedValue)

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

tellIpfsStatus :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Event t IpfsSaveStatus
  -> m ()
tellIpfsStatus ev = tellEvent $ (singletonL . Ipfs) <$> ev

-- it added to base from 4.15.0.0
-- we are on base-4.12.0.0
singletonL :: a -> [a]
singletonL x = [x]