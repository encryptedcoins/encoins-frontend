module ENCOINS.Common.Events where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Data.Time              (NominalDiffTime)
import           Reflex.Dom

import           ENCOINS.Common.Utils   (toText)
import           JS.Website             (logInfo)

newEvent :: MonadWidget t m => m (Event t ())
newEvent = dyn $ constDyn blank

newEventWithDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
newEventWithDelay t = newEvent >>= delay t

logEvent :: (MonadWidget t m, Show a) => Text -> Event t a -> m ()
logEvent title e = performEvent_ $ liftIO . logInfo . ((title <> ": ") <>) . toText <$> e