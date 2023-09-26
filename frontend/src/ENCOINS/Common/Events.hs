module ENCOINS.Common.Events where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Data.Time              (NominalDiffTime)
import           Reflex.Dom

import           ENCOINS.Common.Utils   (toText)
import           JS.Website             (logInfo)
import Language.Javascript.JSaddle (MonadJSM, toJSVal, (#), liftJSM)

newEvent :: MonadWidget t m => m (Event t ())
newEvent = dyn $ constDyn blank

newEventWithDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
newEventWithDelay t = newEvent >>= delay t

logEvent :: (MonadWidget t m, Show a) => Text -> Event t a -> m ()
logEvent title e = performEvent_ $ liftIO . logInfo . ((title <> ": ") <>) . toText <$> e

addFocus :: MonadJSM m
  => InputElement EventResult GhcjsDomSpace t
  -> m ()
addFocus htmlEl = liftJSM $ do
  let rawElement = _inputElement_raw htmlEl
  _ <- toJSVal rawElement # ("focus" :: Text) $ ()
  _ <- toJSVal rawElement # ("select" :: Text) $ ()
  pure ()

addFocusPostBuildDelay :: MonadWidget t m
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
addFocusPostBuildDelay htmlEl = do
  postBuildDelay <- postDelay 0.3
  performEvent_ $ addFocus htmlEl <$ postBuildDelay

-- Useful for adding focus to input element after window opened (event)
addFocusPostBuildDelayE :: MonadWidget t m
  => InputElement EventResult (DomBuilderSpace m) t
  -> Event t ()
  -> m ()
addFocusPostBuildDelayE htmlEl ev = do
  pDelay <- postDelay 0.2
  evDelay <- delay 0.2 ev
  performEvent_ $ addFocus htmlEl <$ (evDelay <> pDelay)

postDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
postDelay seconds = do
  postBuild <- getPostBuild
  delay seconds postBuild