module Widgets.Events where

import           Data.Time       (NominalDiffTime)
import           Reflex.Dom

newEvent :: MonadWidget t m => m (Event t ())
newEvent = dyn $ constDyn blank

newEventWithDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
newEventWithDelay t = newEvent >>= delay t