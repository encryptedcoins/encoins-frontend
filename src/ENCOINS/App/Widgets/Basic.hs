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

btnApp :: MonadWidget t m => Text -> m () -> m (Event t ())
btnApp cls tags = do
    (e, _) <- elAttr' "a" ("href" =: "#" <> "class" =: "app-button  w-button " `Text.append` cls) tags
    return $ () <$ domEvent Click e

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
