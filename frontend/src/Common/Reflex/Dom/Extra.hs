module Common.Reflex.Dom.Extra where

import Data.Text (Text)
import Reflex.Dom

-- Element containing the result of a JavaScript computation
elementResultJS :: (MonadWidget t m) => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f =
    fmap (fmap f . value) $
        inputElement $
            def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId