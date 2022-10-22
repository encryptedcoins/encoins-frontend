module Widgets.Buttons where

import           Reflex.Dom

submitButton :: MonadWidget t m => m ()
submitButton = divClass "" $ do
    blank