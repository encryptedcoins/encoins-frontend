module Widgets.App where

import           Data.Text               (Text)
import           Reflex.Dom              hiding (Input)

import           Backend.EncoinsTx       (encoinsTx)

appWidget :: MonadWidget t m => m ()
appWidget = divClass "" $ do
    mainForm

columnClass :: Text
columnClass = ""

mainForm :: MonadWidget t m => m ()
mainForm = divClass columnClass . divClass "" $ mdo
    encoinsTx