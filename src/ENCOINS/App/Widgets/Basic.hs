module ENCOINS.App.Widgets.Basic where

import           Data.Bool                (bool)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Reflex.Dom
import           Reflex.ScriptDependent   (widgetHoldUntilDefined)

sectionApp :: MonadWidget t m => Text -> Text -> m a -> m a
sectionApp elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section-app wf-section " `Text.append` cls)

containerApp :: MonadWidget t m => Text -> m a -> m a
containerApp cls = divClass ("container-app w-container " `Text.append` cls)

btnApp :: MonadWidget t m => Dynamic t Text -> m () -> m (Event t ())
btnApp dCls tags = do
    let f cls = "href" =: "#" <> "class" =: "app-button  w-button " `Text.append` cls
    (e, _) <- elDynAttr' "a" (fmap f dCls) tags
    return $ () <$ domEvent Click e

imageApp :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> Text -> m (Event t ())
imageApp dFile dCls w =
  let f file cls = "src" =: "images/" `Text.append` file <> "loading" =: "lazy" <> "alt" =: ""
        <> "class" =: cls <> "style" =: "width: " `Text.append` w `Text.append` ";"
  in domEvent Click . fst <$> elDynAttr' "img" (f <$> dFile <*> dCls) blank

copyButtonApp :: MonadWidget t m => m (Event t ())
copyButtonApp = mdo
  let mkClass = bool "copy-div" "tick-div inverted"
  e <- domEvent Click . fst <$> elDynClass' "div" (fmap mkClass d) blank
  e' <- delay 5 e
  d <- holdDyn False $ leftmost [True <$ e, False <$ e']
  return e

checkboxApp :: MonadWidget t m => m (Dynamic t Bool)
checkboxApp = mdo
  let mkClass = bool "checkbox-div" "checkbox-div checkbox-selected"
  (e, _) <- elDynClass' "div" (fmap mkClass d) blank
  d <- toggle False $ domEvent Click e
  return d

waitForScripts :: MonadWidget t m => m () -> m () -> m ()
waitForScripts placeholderWidget actualWidget = do
  ePB <- getPostBuild
  _ <- widgetHoldUntilDefined "walletAPI" ("js/ENCOINS.js" <$ ePB) placeholderWidget actualWidget
  blank
