module ENCOINS.Website.Widgets.Advanced where

import           Reflex.Dom

import           ENCOINS.Website.Widgets.Basic (image)

logo :: MonadWidget t m => m ()
logo = image "logo.svg" "logo inverted" ""
    