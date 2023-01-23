module ENCOINS.Website.Widgets.Basic where

import           Data.Bool              (bool)
import           Data.Text              (Text, unpack)
import qualified Data.Text              as Text
import           Reflex.Dom

section :: MonadWidget t m => Text -> Text -> m a -> m a
section elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section wf-section " `Text.append` cls)

container :: MonadWidget t m => Text -> m a -> m a
container cls = divClass ("container w-container " `Text.append` cls)

h1 :: MonadWidget t m => Text -> m ()
h1 = elClass "h1" "h1" . text

h1Mini :: MonadWidget t m => Text -> m ()
h1Mini = elClass "h1" "h1 h1-mini" . text

h2 :: MonadWidget t m => Text -> m ()
h2 = elClass "h2" "h2" . text

h3 :: MonadWidget t m => Text -> m ()
h3 = elClass "h3" "h3" . text

h4 :: MonadWidget t m => Text -> m ()
h4 = elClass "h4" "h4" . text

h4Invisible :: MonadWidget t m => Text -> m ()
h4Invisible = elClass "h4" "h4 h-invisible" . text

h5 :: MonadWidget t m => Text -> m ()
h5 = elClass "h5" "h5" . text

h5Bold :: MonadWidget t m => Text -> m ()
h5Bold = elClass "h5" "h5 h-extra-bold" . text

h6 :: MonadWidget t m => Text -> m ()
h6 = elClass "h6" "h6" . text

pClass :: MonadWidget t m => Text -> m () -> m ()
pClass = elClass "p"

btn :: MonadWidget t m => Text -> Text -> m (Event t ())
btn cls txt = do
    (e, _) <- elAttr' "a" ("href" =: "#" <> "class" =: "button  w-button " `Text.append` cls) $ text txt
    return $ () <$ domEvent Click e

lnk :: MonadWidget t m => Text -> Text -> m () -> m (Event t ())
lnk ref cls tags = do
    let attrExternal = bool mempty ("target" =: "_blank") (head (unpack ref) /= '#')
    (e, _) <- elAttr' "a" ("href" =: ref <> "class" =: "link w-inline-block " `Text.append` cls <> attrExternal) tags
    return $ () <$ domEvent Click e

lnkInline :: MonadWidget t m => Text -> Text -> m ()
lnkInline ref = elAttr "a" ("href" =: ref <> "class" =: "link-inline" <> "target" =: "_blank") . el "strong" . text

image :: MonadWidget t m => Text -> Text -> Text -> m ()
image file cls w = elAttr "img" ("src" =: "images/" `Text.append` file <> "loading" =: "lazy" <> "alt" =: ""
        <> "class" =: cls <> "style" =: "width: " `Text.append` w `Text.append` ";") blank