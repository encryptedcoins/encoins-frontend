module Common.Reflex.Dom.Extra where

import I18n.Reflex.I18n (HasI18n (localizeWith), HasLocale (askLocale))

import Control.Monad (void)
import Data.Text (Text)
import Reflex.Dom

-- Element containing the result of a JavaScript computation
elementResultJS :: (MonadWidget t m) => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f =
    fmap (fmap f . value) $
        inputElement $
            def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId

dynTextLocale ::
    forall locale term t m.
    ( DomBuilder t m
    , PostBuild t m
    , HasI18n locale term Text
    , HasLocale t locale m
    ) =>
    Dynamic t term
    -> m ()
dynTextLocale = dynTextLocale' localizeWith

dynTextLocale' ::
    (PostBuild t1 m, HasLocale t1 locale m, DomBuilder t1 m) =>
    (locale -> a -> Text)
    -> Dynamic t1 a
    -> m ()
dynTextLocale' f termDyn = do
    localeDyn <- askLocale
    void $
        dyn $
            ffor localeDyn $ \locale ->
                dynText (f locale <$> termDyn)

textLocale ::
    forall locale term t m.
    ( DomBuilder t m
    , PostBuild t m
    , HasI18n locale term Text
    , HasLocale t locale m
    ) =>
    term
    -> m ()
textLocale = textLocale' localizeWith

textLocale' ::
    (PostBuild t1 m, HasLocale t1 locale m, DomBuilder t1 m) =>
    (locale -> a -> Text)
    -> a
    -> m ()
textLocale' f term = do
    localeDyn <- askLocale
    void $
        dyn $
            ffor localeDyn $ \locale ->
                text (f locale term)