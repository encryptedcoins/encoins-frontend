module ENCOINS.Common.Widgets.Basic where

import           Data.Bool              (bool)
import           Data.Text              (Text, unpack)
import qualified Data.Text              as T
import           Reflex.Dom

import           Backend.Status (Status(..))

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

btn :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m () -> m (Event t ())
btn dCls dStyle tags = do
    let f cls style = "href" =: "#" <> "class" =: "app-button  w-button " `T.append` cls <> "style" =: style
    (e, _) <- elDynAttr' "a" (zipDynWith f dCls dStyle) tags
    return $ () <$ domEvent Click e

btnWithBlock :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t Text
  -> Dynamic t Bool
  -> m ()
  -> m (Event t ())
btnWithBlock dCls dStyle dIsBlock = btn
    (mkBtnAttrs dIsBlock)
    dStyle
  where
    mkBtnAttrs dSt = do
      defaultClass <- dCls
      let classWithDisable = defaultClass <> space <> "button-disabled"
      bool defaultClass classWithDisable <$> dSt

btnExternal :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> Dynamic t Text -> m () -> m (Event t ())
btnExternal dRef dCls dStyle tags = do
    let f ref cls style = "href" =: ref <> "class" =: "app-button  w-button " `T.append` cls <> "style" =: style
    (e, _) <- elDynAttr' "a" (f <$> dRef <*> dCls <*> dStyle) tags
    return $ () <$ domEvent Click e

lnk :: MonadWidget t m => Text -> Text -> m () -> m (Event t ())
lnk ref cls tags = do
    let attrExternal = bool mempty ("target" =: "_blank") (head (unpack ref) /= '#')
    (e, _) <- elAttr' "a" ("href" =: ref <> "class" =: "link w-inline-block " `T.append` cls <> attrExternal) tags
    return $ () <$ domEvent Click e

lnkInline :: MonadWidget t m => Text -> Text -> m ()
lnkInline ref = elAttr "a" ("href" =: ref <> "class" =: "link-inline" <> "target" =: "_blank") . el "strong" . text

lnkInlineInverted :: MonadWidget t m => Text -> Text -> m ()
lnkInlineInverted ref = elAttr "a" ("href" =: ref <> "class" =: "link-inline inverted" <> "target" =: "_blank") . el "strong" . text

image :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> Text -> m (Event t ())
image dFile dCls w =
  let f file cls = "src" =: "images/" `T.append` file <> "loading" =: "lazy" <> "alt" =: ""
        <> "class" =: cls <> "style" =: "width: " `T.append` w `T.append` ";"
  in domEvent Click . fst <$> elDynAttr' "img" (f <$> dFile <*> dCls) blank

br :: MonadWidget t m => m ()
br = el "br" blank

divClassId :: MonadWidget t m => Text -> Text -> m a -> m a
divClassId cls elId = elAttr "div" ("class" =: cls <> "id" =: elId)

errDiv :: MonadWidget t m => Text -> m ()
errDiv = elAttr "div" ("class" =: "w-file-upload-error w-file-upload-error-msg"
  <> "style" =: "margin-top: 0px;margin-bottom: 10px;") . text

space :: Text
space = " "

column :: Text
column = ":"

notification :: MonadWidget t m => Dynamic t Text -> m ()
notification dNotification = do
  divClass "notification" $ do
    divClass "notification-text" $ dynText dNotification

-- Other error element
otherStatus :: MonadWidget t m => Event t Text -> m (Event t Status)
otherStatus eOtherError = do
    let eOtherStatusNonEmpty = ffilter ("" /=) eOtherError
    return $ CustomStatus <$> eOtherStatusNonEmpty