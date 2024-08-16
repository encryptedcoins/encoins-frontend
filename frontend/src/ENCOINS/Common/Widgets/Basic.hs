module ENCOINS.Common.Widgets.Basic where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Reflex.Dom

import Common.Utility (space)
import I18n.I18n (App)
import I18n.Common.I18n (HasI18n, Locale)
import Common.Reflex.Dom.Extra (textLocale)

h1 :: (MonadWidget t m) => Text -> m ()
h1 = elClass "h1" "h1" . text

h1Mini :: (MonadWidget t m) => Text -> m ()
h1Mini = elClass "h1" "h1 h1-mini" . text

h1Locale :: (App t m, HasI18n Locale a Text) => a -> m ()
h1Locale = elClass "h1" "h1" . textLocale

h2 :: (MonadWidget t m) => Text -> m ()
h2 = elClass "h2" "h2" . text

h2Locale :: (App t m, HasI18n Locale a Text) => a -> m ()
h2Locale = elClass "h2" "h2" . textLocale

h3 :: (MonadWidget t m) => Text -> m ()
h3 = elClass "h3" "h3" . text

h3Locale :: (App t m, HasI18n Locale a Text) => a -> m ()
h3Locale = elClass "h3" "h3" . textLocale

h4 :: (MonadWidget t m) => Text -> m ()
h4 = elClass "h4" "h4" . text

h4Locale :: (App t m, HasI18n Locale a Text) => a -> m ()
h4Locale = elClass "h4" "h4" . textLocale

h4Invisible :: (MonadWidget t m) => Text -> m ()
h4Invisible = elClass "h4" "h4 h-invisible" . text

h5 :: (MonadWidget t m) => Text -> m ()
h5 = elClass "h5" "h5" . text

h5Locale :: (App t m, HasI18n Locale a Text) => a -> m ()
h5Locale = elClass "h5" "h5" . textLocale

h5Bold :: (MonadWidget t m) => Text -> m ()
h5Bold = elClass "h5" "h5 h-extra-bold" . text

h5BoldLocale :: (App t m, HasI18n Locale a Text) => a -> m ()
h5BoldLocale = elClass "h5" "h5 h-extra-bold" . textLocale

h6 :: (MonadWidget t m) => Text -> m ()
h6 = elClass "h6" "h6" . text

pClass :: (MonadWidget t m) => Text -> m () -> m ()
pClass = elClass "p"

logo :: (MonadWidget t m) => m ()
logo = void $ image "logo.svg" "logo inverted" ""

imageButton :: (MonadWidget t m) => Dynamic t Text -> Text -> m (Event t ())
imageButton dFile w = do
    image dFile (pure "w-button") w

btn ::
    (MonadWidget t m) => Dynamic t Text -> Dynamic t Text -> m () -> m (Event t ())
btn dCls dStyle tags = do
    let f cls style =
            "href" =: "#"
                <> "class" =: "app-button  w-button " `T.append` cls
                <> "style" =: style
    (e, _) <- elDynAttr' "a" (zipDynWith f dCls dStyle) tags
    return $ () <$ domEvent Click e

btnWithBlock ::
    (MonadWidget t m) =>
    Dynamic t Text
    -> Dynamic t Text
    -> Dynamic t Bool
    -> m ()
    -> m (Event t ())
btnWithBlock dCls dStyle dIsBlock tags = do
    let f style cls =
            "href" =: "#"
                <> "class" =: "app-button  w-button " `T.append` cls
                <> "style" =: style
    let dBlockBtnCls = do
            defaultClass <- dCls
            let classWithDisable = defaultClass <> space <> "button-disabled"
            bool defaultClass classWithDisable <$> dIsBlock
    (e, _) <- elDynAttr' "a" (zipDynWith f dStyle dBlockBtnCls) tags
    let eGated =
            gate (current $ not <$> dIsBlock) $
                leftmost [() <$ domEvent Click e, keydown Enter e]
    pure eGated

btnWithOverOutBlock ::
    (MonadWidget t m) =>
    Dynamic t Text
    -> Dynamic t Text
    -> Dynamic t Bool
    -> m ()
    -> m (Event t (), Event t (), Event t ())
btnWithOverOutBlock dCls dStyle dIsBlock tags = do
    let f style cls =
            "href" =: "#"
                <> "class" =: "app-button  w-button " `T.append` cls
                <> "style" =: style
    let dBlockBtnCls = do
            defaultClass <- dCls
            let classWithDisable = defaultClass <> space <> "button-disabled"
            bool defaultClass classWithDisable <$> dIsBlock
    (e, _) <- elDynAttr' "a" (zipDynWith f dStyle dBlockBtnCls) tags
    let mouseOver = () <$ domEvent Mouseover e
    let mouseOut = () <$ domEvent Mouseout e
    let eGated =
            gate (current $ not <$> dIsBlock) $
                leftmost [() <$ domEvent Click e, keydown Enter e]
    pure (mouseOver, mouseOut, eGated)

btnExternal ::
    (MonadWidget t m) =>
    Dynamic t Text
    -> Dynamic t Text
    -> Dynamic t Text
    -> m ()
    -> m (Event t ())
btnExternal dRef dCls dStyle tags = do
    let f ref cls style =
            "href" =: ref
                <> "class" =: "app-button  w-button " `T.append` cls
                <> "style" =: style
    (e, _) <- elDynAttr' "a" (f <$> dRef <*> dCls <*> dStyle) tags
    return $ () <$ domEvent Click e

lnk :: (MonadWidget t m) => Text -> Text -> m () -> m (Event t ())
lnk ref cls tags = do
    let attrExternal = bool mempty ("target" =: "_blank") (head (unpack ref) /= '#')
    (e, _) <-
        elAttr'
            "a"
            ( "href" =: ref
                <> "class" =: "link w-inline-block " `T.append` cls
                <> attrExternal
            )
            tags
    return $ () <$ domEvent Click e

lnkInline :: (MonadWidget t m) => Text -> Text -> m ()
lnkInline ref =
    elAttr "a" ("href" =: ref <> "class" =: "link-inline" <> "target" =: "_blank")
        . el "strong"
        . text

lnkInlineInverted :: (MonadWidget t m) => Text -> Text -> m ()
lnkInlineInverted ref =
    elAttr
        "a"
        ("href" =: ref <> "class" =: "link-inline inverted" <> "target" =: "_blank")
        . el "strong"
        . text

image ::
    (MonadWidget t m) => Dynamic t Text -> Dynamic t Text -> Text -> m (Event t ())
image dFile dCls w =
    let f file cls =
            "src" =: "images/" `T.append` file
                <> "loading" =: "lazy"
                <> "alt" =: ""
                <> "class" =: cls
                <> "style" =: "width: " `T.append` w `T.append` ";"
     in domEvent Click . fst <$> elDynAttr' "img" (f <$> dFile <*> dCls) blank

br :: (MonadWidget t m) => m ()
br = el "br" blank

divClassId :: (MonadWidget t m) => Text -> Text -> m a -> m a
divClassId cls elId = elAttr "div" ("class" =: cls <> "id" =: elId)

errDiv :: (MonadWidget t m) => Text -> m ()
errDiv =
    elAttr
        "div"
        ( "class" =: "w-file-upload-error w-file-upload-error-msg"
            <> "style" =: "margin-top: 0px;margin-bottom: 10px;"
        )
        . text

notification :: (MonadWidget t m) => Dynamic t Text -> m ()
notification dNotification = do
    divClass "notification" $ do
        divClass "notification-text" $ dynText dNotification

divClassDyn :: (MonadWidget t m) => Dynamic t Text -> m a -> m a
divClassDyn = elDynClass "div"

sectionApp :: (MonadWidget t m) => Text -> Text -> m a -> m a
sectionApp elemId cls =
    elAttr
        "div"
        ("id" =: elemId <> "class" =: "section-app wf-section " `T.append` cls)

containerApp :: (MonadWidget t m) => Text -> m a -> m a
containerApp cls = divClass ("container-app w-container " `T.append` cls)