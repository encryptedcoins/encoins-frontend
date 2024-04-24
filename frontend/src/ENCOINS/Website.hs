module ENCOINS.Website
    ( website
    ) where

import Reflex.Dom

import ENCOINS.Website.Body (bodyWidget)
import ENCOINS.Website.Head (headWidget)

website :: IO ()
website = mainWidgetWithHead headWidget bodyWidget
