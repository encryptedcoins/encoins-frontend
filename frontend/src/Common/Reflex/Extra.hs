module Common.Reflex.Extra where

import Control.Monad (join, (<=<))
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Reflex.Dom
import Witherable (catMaybes)

switchHoldDyn ::
    (MonadWidget t m) =>
    Dynamic t a
    -> (a -> m (Event t b))
    -> m (Event t b)
switchHoldDyn da f = switchHold never <=< dyn $ da <&> f

dynHoldDyn ::
    (MonadWidget t m) =>
    Dynamic t a
    -> b
    -> (a -> m (Dynamic t b))
    -> m (Dynamic t b)
dynHoldDyn dA b f = do
    edB <- dyn $ f <$> dA
    join <$> holdDyn (constDyn b) edB

eventMaybe :: (Reflex t) => Event t (Maybe a) -> (Event t (), Event t a)
eventMaybe ev = (() <$ ffilter isNothing ev, catMaybes ev)

eventMaybeDynDef ::
    (Reflex t) =>
    Dynamic t def
    -> Event t (Maybe a)
    -> (Event t def, Event t a)
eventMaybeDynDef dDefault ev = (tagPromptlyDyn dDefault $ ffilter isNothing ev, catMaybes ev)

eventTuple :: (Reflex t) => Event t (a, b) -> (Event t a, Event t b)
eventTuple ev = (fst <$> ev, snd <$> ev)

dynTuple ::
    (MonadWidget t m) =>
    a
    -> b
    -> Event t (a, b)
    -> m (Dynamic t a, Dynamic t b)
dynTuple aDef bDef eAB = do
    da <- holdDyn aDef $ fst <$> eAB
    db <- holdDyn bDef $ snd <$> eAB
    pure (da, db)

foldDynamicAny :: (Reflex t) => [Dynamic t Bool] -> Dynamic t Bool
foldDynamicAny = foldr (zipDynWith (||)) (constDyn False)

-- Fire 'Main event' only when there is some value in Condition event.
fireWhenJustThenReset ::
    (MonadWidget t m) =>
    Event t a -- Main event
    -> Event t (Maybe b) -- Condition event
    -> Event t c -- Reset event
    -> m (Event t ())
fireWhenJustThenReset eMain eCondition eReset = do
    -- Hold 'Main event' as True value ,
    -- and then after 'Reset event' fires
    -- reset it to False.
    dIsMain <- holdDyn False $ leftmost [True <$ eMain, False <$ eReset]
    pure $
        attachPromptlyDynWithMaybe
            (\isMain mCondition -> if isMain && isJust mCondition then Just () else Nothing)
            dIsMain
            eCondition

updateUrls ::
    (MonadWidget t m) =>
    Dynamic t [Text]
    -> Event t (Maybe Text)
    -> m (Dynamic t [Text])
updateUrls dUrls eFailedUrl = do
    dFailedUrls <-
        foldDyn (\mUrl acc -> maybe acc (\u -> u : acc) mUrl) [] eFailedUrl
    pure $ zipDynWith (\\) dUrls dFailedUrls
