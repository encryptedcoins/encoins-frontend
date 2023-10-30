module ENCOINS.Common.Widgets.Basic where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import           Data.Text                       (Text, unpack)
import qualified Data.Text                       as T
import           Reflex.Dom
import           Control.Lens ((%~))
import           Data.Proxy             (Proxy (..))
import           Data.Map  (Map)

import           Backend.Status                  (Status (..))
import           ENCOINS.Common.Events

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

logo :: MonadWidget t m => m ()
logo = void $ image "logo.svg" "logo inverted" ""

imageButton :: MonadWidget t m => Dynamic t Text -> Text -> m (Event t ())
imageButton dFile w = do
  image dFile (pure "w-button") w

btn :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m () -> m (Event t ())
btn dCls dStyle tags = do
    let f cls style = "href" =: "#" <> "class" =: "app-button  w-button " `T.append` cls <> "style" =: style
    (e, _) <- elDynAttrPrevDef Keydown "a" (zipDynWith f dCls dStyle) tags
    return $ () <$ domEvent Click e

btnWithBlock :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t Text
  -> Dynamic t Bool
  -> m ()
  -> m (Event t ())
btnWithBlock dCls dStyle dIsBlock = btn (mkBtnAttrs dIsBlock) dStyle
  where
    mkBtnAttrs dBlock = do
      defaultClass <- dCls
      let classWithDisable = defaultClass <> space <> "button-disabled"
      bool defaultClass classWithDisable <$> dBlock

btn' :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t Text
  -> m ()
  -> m (Event t ())
btn' dCls dStyle tags = do
    let f cls style = "href" =: "#" <> "class" =: "app-button  w-button " `T.append` cls <> "style" =: style
    (e, _) <- elDynAttrPrevDef Keypress "a" (zipDynWith f dCls dStyle) tags
    pure $ leftmost [() <$ domEvent Click e, keypress Enter e]

btnWithBlock' :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t Text
  -> Dynamic t Bool
  -> m ()
  -> m (Event t ())
btnWithBlock' dCls dStyle dIsBlock tags = do
  e <- btn' (mkBtnAttrs dIsBlock) dStyle tags
  -- let eClick = () <$ domEvent Click e
  logEvent "e" e
  -- let eEnter = keydown Enter e
  -- logEvent "eEnter" eEnter
  let dIsNotBlock = not <$> dIsBlock
  logDyn "dIsNotBlock" dIsNotBlock
  let eGated = gate (current dIsNotBlock) e
  logEvent "eGated" eGated
  pure eGated
  where
    mkBtnAttrs dBlock = do
      defaultClass <- dCls
      let classWithDisable = defaultClass <> space <> "button-disabled"
      bool defaultClass classWithDisable <$> dBlock

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

-- | From https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7
--   Like 'elDynAttr'' but configures "prevent default" on the given event.
--   Blocks for example a context menu from poping up on right mouse click.
--   This should be used with caution, as it may be unexpected for end user.
elDynAttrPrevDef
  :: forall a en m t
   . (DomBuilder t m, PostBuild t m)
  => EventName en -- ^ Event on the element to configure with 'preventDefault'
  -> Text -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a -- ^ Child of element
  -> m (Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrPrevDef ev = elDynAttrModConf
  (\elCfg -> elCfg & elementConfig_eventSpec %~ addEventSpecFlags
    (Proxy :: Proxy (DomBuilderSpace m))
    ev
    -- (const preventDefault)
    (const stopPropagation)
  )

-- | Like 'elDynAttr'' but allows you to modify the element configuration.
--
-- Special thanks to @luigy: https://gist.github.com/luigy/b49ce04de8462e594c9c2b5b455ae5a5#file-foo-hs
elDynAttrModConf
  :: (DomBuilder t m, PostBuild t m)
  => (  ElementConfig EventResult t (DomBuilderSpace m)
     -> ElementConfig EventResult t (DomBuilderSpace m)
     )
  -> Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrModConf f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg =
        def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result    <- element elementTag (f cfg) child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result