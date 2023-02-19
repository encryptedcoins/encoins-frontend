module ENCOINS.Common.Widgets.Advanced where

import           Control.Monad                (void)
import           Data.Bool                    (bool)
import           Data.Text                    (Text)
import           Reflex.Dom

import           ENCOINS.Common.Widgets.Basic (image)

logo :: MonadWidget t m => m ()
logo = void $ image "logo.svg" "logo inverted" ""

imageButton :: MonadWidget t m => Dynamic t Text -> Text -> m (Event t ())
imageButton dFile w = do
  image dFile (pure "w-button") w

copyButton :: MonadWidget t m => m (Event t ())
copyButton = mdo
  let mkClass = bool "copy-div" "tick-div inverted"
  e  <- domEvent Click . fst <$> elDynClass' "div" (fmap mkClass d) blank
  e' <- delay 5 e
  d  <- holdDyn False $ leftmost [True <$ e, False <$ e']
  return e

checkboxButton :: MonadWidget t m => m (Dynamic t Bool)
checkboxButton = mdo
  let mkClass = bool "checkbox-div" "checkbox-div checkbox-selected"
  (e, _) <- elDynClass' "div" (fmap mkClass d) blank
  d <- toggle False $ domEvent Click e
  return d

dialogWindow :: MonadWidget t m => Dynamic t Bool -> Text -> m a -> m a
dialogWindow dWindowIsOpen style tags = 
  let mkClass b = "class" =: "dialog-window-wrapper" <> bool ("style" =: "display: none") mempty b
  in elDynAttr "div" (fmap mkClass dWindowIsOpen) $ elAttr "div"
      ("class" =: "dialog-window" <> "style" =: style) tags