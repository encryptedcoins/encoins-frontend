module Widgets.Events where

import           Reflex.Dom
import Data.Time (NominalDiffTime)

newEvent :: MonadWidget t m => m (Event t ())
newEvent = dyn $ constDyn blank

newEventWithDelay :: MonadWidget t m => NominalDiffTime -> m (Event t ())
newEventWithDelay t = newEvent >>= delay t