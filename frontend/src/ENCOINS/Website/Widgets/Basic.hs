module ENCOINS.Website.Widgets.Basic where

import           Data.Text  (Text)
import qualified Data.Text  as Text
import           Reflex.Dom

section :: MonadWidget t m => Text -> Text -> m a -> m a
section elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section wf-section " `Text.append` cls)

container :: MonadWidget t m => Text -> m a -> m a
container cls = divClass ("container w-container " `Text.append` cls)
