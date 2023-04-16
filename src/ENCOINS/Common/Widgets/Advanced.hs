module ENCOINS.Common.Widgets.Advanced where

import           Control.Monad                (void)
import           Data.Bool                    (bool)
import           Data.Text                    (Text)
import           Data.Time                    (NominalDiffTime)
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
  notification d (text "Copied!")
  return e

notification :: MonadWidget t m => Dynamic t Bool -> m () -> m ()
notification dVis innerW = dyn_ $ bool blank (divClass "bottom-notification" .
  divClass "notification-content" $ innerW) <$> dVis

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

withTooltip :: MonadWidget t m =>
  m a ->
  -- ^ Element that triggers tooltip when hovered
  Text ->
  -- ^ Additional styles for the tooltip
  NominalDiffTime ->
  -- ^ Emersion delay in seconds, >= 0
  NominalDiffTime ->
  -- ^ Vanishing delay in seconds, >= 0
  m () ->
  -- ^ Inner content of the tooltip
  m a
withTooltip mainW style delay1 delay2 innerW = mdo
  (e, ret) <- elClass' "div" "div-tooltip-wrapper" $ do
    ret' <- mainW
    let
      eMouseIn = traceEvent "eMouseIn" $ domEvent Mouseenter e
      eMouseOut = traceEvent "eMouseOut" $ domEvent Mouseleave e
    eShow <- delay delay1 eMouseIn
    eHide <- delay delay2 eMouseOut
    dAttrs <- holdDyn hideAttrs $ leftmost [showAttrs <$ traceEvent "eShow" eShow,
      hideAttrs <$ traceEvent "eHide" eHide]
    elDynAttr "div" (traceDyn "dAttrs" dAttrs) innerW
    return ret'
  return ret
  where
    constAttrs = "class" =: "div-tooltip top"
    showAttrs = constAttrs <> "style" =: ("display:inline-block;" <> style)
    hideAttrs = constAttrs <> "style" =: ("display:none;" <> style)
