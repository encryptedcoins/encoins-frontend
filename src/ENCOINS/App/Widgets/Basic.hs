module ENCOINS.App.Widgets.Basic where

import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Reflex.Dom
import           Reflex.ScriptDependent (widgetHoldUntilDefined)

sectionApp :: MonadWidget t m => Text -> Text -> m a -> m a
sectionApp elemId cls = elAttr "div" ("id" =: elemId <> "class" =: "section-app wf-section " `Text.append` cls)

containerApp :: MonadWidget t m => Text -> m a -> m a
containerApp cls = divClass ("container-app w-container " `Text.append` cls)

btnApp :: MonadWidget t m => Text -> Text -> m (Event t ())
btnApp cls txt = do
    (e, _) <- elAttr' "a" ("href" =: "#" <> "class" =: "app-button  w-button " `Text.append` cls) $ text txt
    return $ () <$ domEvent Click e

waitForScripts :: MonadWidget t m => m () -> m () -> m ()
waitForScripts placeholderWidget actualWidget = do
  ePB <- getPostBuild
  _ <- widgetHoldUntilDefined "walletAPI" ("js/ENCOINS.js" <$ ePB) placeholderWidget actualWidget
  blank