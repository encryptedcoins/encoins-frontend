module ENCOINS.App
    ( app
    ) where

import Reflex.Dom

import ENCOINS.App.Body (bodyWidget)
import ENCOINS.Common.Head (headWidget)

app :: IO ()
app = mainWidgetWithHead headWidget bodyWidget
