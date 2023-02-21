module ENCOINS.App.Widgets.Basic where

import           Data.Aeson               (FromJSON, decodeStrict)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (encodeUtf8)
import           Reflex.Dom
import           Reflex.ScriptDependent   (widgetHoldUntilDefined)

import           JS.Website               (loadJSON)
import           Widgets.Events           (newEventWithDelay)

sectionApp :: MonadWidget t m => Text -> Text -> m a -> m a
sectionApp elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section-app wf-section " `Text.append` cls)

containerApp :: MonadWidget t m => Text -> m a -> m a
containerApp cls = divClass ("container-app w-container " `Text.append` cls)

-- Element containing the result of a JavaScript computation
elementResultJS :: MonadWidget t m => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f = fmap (fmap f . value) $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId

waitForScripts :: MonadWidget t m => m () -> m () -> m ()
waitForScripts placeholderWidget actualWidget = do
  ePB <- getPostBuild
  _ <- widgetHoldUntilDefined "walletAPI" ("js/ENCOINS.js" <$ ePB) placeholderWidget actualWidget
  blank

loadAppData :: forall t m a b . (MonadWidget t m, FromJSON a) => Text -> (a -> b) -> b -> m (Dynamic t b)
loadAppData entry f val = do
    let elId = "elId-" <> entry
    e <- newEventWithDelay 0.1
    performEvent_ (loadJSON entry elId <$ e)
    elementResultJS elId (maybe val f . (decodeStrict :: ByteString -> Maybe a) . encodeUtf8)