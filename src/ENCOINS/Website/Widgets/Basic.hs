module ENCOINS.Website.Widgets.Basic where

import           Control.Monad          (when)
import           Data.Bool              (bool)
import           Data.List              (elemIndex)
import           Data.Map               (Map)
import           Data.Text              (Text, unpack)
import qualified Data.Text              as Text
import           Reflex.Dom
import           Text.Read              (readMaybe)
import           Witherable             (catMaybes)

import           Widgets.Utils          (toText, safeIndex)

section :: MonadWidget t m => Text -> m a -> m a
section elemId = elAttr "div" ("id" =: elemId <> "class" =: "section wf-section")

container :: MonadWidget t m => Text -> m a -> m a
container cls = divClass ("container w-container " `Text.append` cls)

h1 :: MonadWidget t m => Text -> m ()
h1 = elClass "h1" "h1" . text

h2 :: MonadWidget t m => Text -> m ()
h2 = elClass "h2" "h2" . text

h3 :: MonadWidget t m => Text -> m ()
h3 = elClass "h3" "h3" . text

h4 :: MonadWidget t m => Text -> m ()
h4 = elClass "h4" "h4" . text

h5 :: MonadWidget t m => Text -> m ()
h5 = elClass "h5" "h5" . text

h6 :: MonadWidget t m => Text -> m ()
h6 = elClass "h6" "h6" . text

pClass :: MonadWidget t m => Text -> m () -> m ()
pClass = elClass "p"

btn :: MonadWidget t m => Text -> Text -> m (Event t ())
btn cls txt = do
    (e, _) <- elAttr' "a" ("href" =: "#" <> "class" =: "button  w-button " `Text.append` cls) $ text txt
    return $ () <$ domEvent Click e

image :: MonadWidget t m => Text -> Text -> Text -> m ()
image file cls w = elAttr "img" ("src" =: "images/" `Text.append` file <> "loading" =: "lazy" <> "alt" =: ""
        <> "class" =: cls <> "style" =: "width: " `Text.append` w `Text.append` ";") blank

resourseButton :: MonadWidget t m => Text -> Text -> Text -> Text -> m ()
resourseButton cls lnk file w = divClass "div-image-large" $
    elAttr "a" ("href" =: lnk <> "target" =: "_blank" <> "class" =: "link w-inline-block") $
    image file ("image " `Text.append` cls) w