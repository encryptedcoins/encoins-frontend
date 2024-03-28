module ENCOINS.Common.Events where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import           Data.Time                   (NominalDiffTime)
import           Reflex.Dom

import           Backend.Utility             (toText)
import           JS.Website                  (logInfo)
import           Language.Javascript.JSaddle (MonadJSM, liftJSM, toJSVal, (#))

newEvent :: MonadWidget t m => m (Event t ())
newEvent = dyn $ constDyn blank

newEventWithDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
newEventWithDelay t = newEvent >>= delay t

logEvent :: (MonadWidget t m, Show a) => Text -> Event t a -> m ()
logEvent title e = performEvent_ $ liftIO . logInfo . ((title <> ": \n") <>) . toText <$> e

logDyn :: (MonadWidget t m, Show a) => Text -> Dynamic t a -> m ()
logDyn title d = logEvent title $ updated d

setFocus :: MonadJSM m
  => InputElement EventResult GhcjsDomSpace t
  -> m ()
setFocus htmlEl = liftJSM $ do
  let rawElement = _inputElement_raw htmlEl
  _ <- toJSVal rawElement # ("focus" :: Text) $ ()
  _ <- toJSVal rawElement # ("select" :: Text) $ ()
  pure ()

setFocusDelay :: MonadWidget t m
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
setFocusDelay htmlEl = do
  postBuildDelay <- postDelay 0.3
  performEvent_ $ setFocus htmlEl <$ postBuildDelay

-- Useful for adding focus to input element after window opened (event)
setFocusDelayOnEvent :: MonadWidget t m
  => InputElement EventResult (DomBuilderSpace m) t
  -> Event t ()
  -> m ()
setFocusDelayOnEvent htmlEl ev = do
  ePostDelay <- postDelay 0.2
  eDelay <- delay 0.2 ev
  performEvent_ $ setFocus htmlEl <$ (eDelay <> ePostDelay)

postDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
postDelay seconds = do
  postBuild <- getPostBuild
  delay seconds postBuild
