module Widgets.CoinEntry where

import           Reflex.Dom

-- A checkbox to select a coin entry
coinEntrySelectWidget :: MonadWidget t m => m ()
coinEntrySelectWidget = divClass "" $ do
    blank 

coinEntryWidget :: MonadWidget t m => m ()
coinEntryWidget = divClass "" $ do
    blank

-- A button that adds a coinEntryWidget
coinEntryAddWidget :: MonadWidget t m => m ()
coinEntryAddWidget = divClass "" $ do
    blank

-- A list of coinEntryWidgets
coinCollectionWidget :: MonadWidget t m => m ()
coinCollectionWidget = divClass "" $ do
    blank