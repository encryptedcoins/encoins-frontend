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

dynText_ ::
    forall locale term t m.
    ( DomBuilder t m
    , PostBuild t m
    , HasI18n locale term Text
    , HasLocale t locale m
    ) =>
    Dynamic t term
    -> m ()
dynText_ = dynText'_ localizeWith

dynText'_ ::
    (PostBuild t1 m, HasLocale t1 locale m, DomBuilder t1 m) =>
    (locale -> a -> Text)
    -> Dynamic t1 a
    -> m ()
dynText'_ f termDyn = do
    localeDyn <- askLocale
    void $
        dyn $
            ffor localeDyn $ \locale ->
                dynText (f locale <$> termDyn)
