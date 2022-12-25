module Widgets.CoinEntry where

import           Reflex.Dom

import           ENCOINS.BaseTypes           (MintingPolarity (..))
import           ENCOINS.Bulletproofs        (Secret (..))
import           ENCOINS.Crypto.Field        (Field(..))

coinEntryWidget :: MonadWidget t m => (Secret, MintingPolarity) -> m (Dynamic t (Secret, MintingPolarity))
coinEntryWidget a = divClass "" $ do
    return $ constDyn a

-- A list of coinEntryWidgets
coinCollectionWidget :: MonadWidget t m => m (Dynamic t [(Secret, MintingPolarity)])
coinCollectionWidget = divClass "" $ do
    d1 <- coinEntryWidget (Secret (F 78623591232) (F 3), Mint)
    d2 <- coinEntryWidget (Secret (F 21879124) (F 5), Mint)
    return $ sequenceA [d1, d2]

-------------------------------------------------------

-- A checkbox to select a coin entry
coinEntrySelectWidget :: MonadWidget t m => m ()
coinEntrySelectWidget = divClass "" $ do
    blank 

-- A button that adds a coinEntryWidget
coinEntryAddWidget :: MonadWidget t m => m ()
coinEntryAddWidget = divClass "" $ do
    blank