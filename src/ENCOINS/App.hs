module ENCOINS.App (app) where

import           Reflex.Dom

import           ENCOINS.Website.Head        (headWidget)
import           ENCOINS.App.Body            (bodyWidget)

app :: IO ()
app = mainWidgetWithHead headWidget bodyWidget