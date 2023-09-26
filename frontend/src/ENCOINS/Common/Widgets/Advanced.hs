module ENCOINS.Common.Widgets.Advanced where

import           Control.Monad                 (void)
import           Data.Bool                     (bool)
import           Data.Text                     (Text)
import           Data.Time                     (NominalDiffTime)
import           GHCJS.DOM                     (currentDocumentUnchecked)
import           GHCJS.DOM.EventM              (on, target)
import           GHCJS.DOM.GlobalEventHandlers (click)
import           GHCJS.DOM.Node                (contains)
import qualified GHCJS.DOM.Types               as DOM

import           Reflex.Dom

import           ENCOINS.Common.Widgets.Basic  (image)
import           JS.Website                    (setElementStyle)
import           Backend.Wallet (NetworkId)
import           ENCOINS.Common.Utils (toText)

logo :: MonadWidget t m => m ()
logo = void $ image "logo.svg" "logo inverted" ""

imageButton :: MonadWidget t m => Dynamic t Text -> Text -> m (Event t ())
imageButton dFile w = do
  image dFile (pure "w-button") w

copyEvent :: MonadWidget t m => Event t () -> m (Dynamic t Bool)
copyEvent e = do
  e' <- delay 5 e
  d <- holdDyn False $ leftmost [True <$ e, False <$ e']
  performEvent_ (setElementStyle "bottom-notification-copy" "display" "flex" <$ e)
  performEvent_ (setElementStyle "bottom-notification-copy" "display" "none" <$ e')
  return d

copyButton :: MonadWidget t m => m (Event t ())
copyButton = mdo
  let mkClass = bool "copy-div" "tick-div inverted"
  e <- domEvent Click . fst <$> elDynClass' "div" (fmap mkClass d) blank
  d <- copyEvent e
  return e

copiedNotification :: MonadWidget t m => m ()
copiedNotification = elAttr "div" ("class" =: "bottom-notification" <>
  "id" =: "bottom-notification-copy" <> "style" =: "display:none;") .
  divClass "notification-content" $ text "Copied!"

noRelayNotification :: MonadWidget t m => m ()
noRelayNotification = elAttr "div" ("class" =: "bottom-notification" <>
  "id" =: "bottom-notification-relay" <> "style" =: "display:none;") .
  divClass "notification-content" $ text
    "All available relays are down! Try reloading the page or come back later."

wrongNetworkNotification :: MonadWidget t m => NetworkId -> m ()
wrongNetworkNotification network = elAttr "div"
  (  "class" =: "bottom-notification"
  <> "id" =: "bottom-notification-network"
  <> "style" =: "display:none;"
  )
  . divClass "notification-content"
  $ text
  $ "Wrong network! Please switch to the "
  <> toText network
  <> "."

checkboxButton :: MonadWidget t m => m (Dynamic t Bool)
checkboxButton = mdo
  let mkClass = bool "checkbox-div" "checkbox-div checkbox-selected"
  (e, _) <- elDynClass' "div" (fmap mkClass d) blank
  d <- toggle False $ domEvent Click e
  return d

dialogWindow :: MonadWidget t m => Bool -> Event t () -> Event t () -> Text -> Text -> m a -> m a
dialogWindow close eOpen eClose style title tags = mdo
  eClickOuside <- if close
      then clickOutside (_element_raw e)
      else pure never
  -- Delay prevents from closing because eClickOuside fires
  eOpenDelayed <- delay 0.1 eOpen
  let
    mkClass b = "class" =: "dialog-window-wrapper" <> bool ("style" =: "display: none") mempty b
    eClose' = leftmost [eClose, eClickOuside, eCross]
  dWindowIsOpen <- holdDyn False $ leftmost [True <$ eOpenDelayed, False <$ eClose']
  (e, (ret, eCross)) <- elDynAttr "div" (fmap mkClass dWindowIsOpen) $
      elAttr' "div" ("class" =: "dialog-window" <> "style" =: style) $ do
        crossClick <- divClass "dialog-window-title" $ do
          elAttr "div" ("style" =: "width: 20px;") blank
          divClass "app-text-semibold" $ text title
          if close
            then domEvent Click . fst <$> elClass' "div" "cross-div inverted" blank
            else pure never
        (,crossClick) <$> tags
  return ret

clickOutside :: MonadWidget t m => DOM.Element -> m (Event t ())
clickOutside elm = do
  doc <- currentDocumentUnchecked
  ev <- wrapDomEvent doc (`on` click) $ do
    t <- target
    maybe (pure False) (isParentOf elm) t
  pure $ fforMaybe ev $ \inside1 -> if inside1 then Nothing else Just ()

isParentOf :: DOM.MonadJSM m => DOM.Element -> DOM.Element -> m Bool
isParentOf parent node = DOM.liftJSM $ contains parent (Just node)

withTooltip :: MonadWidget t m =>
     m a
  -- ^ Element that triggers tooltip when hovered
  -> Text
  -- ^ Additional styles for the tooltip
  -> NominalDiffTime
  -- ^ Emersion delay in seconds, >= 0
  -> NominalDiffTime
  -- ^ Vanishing delay in seconds, >= 0
  -> m ()
  -- ^ Inner content of the tooltip
  -> m a
withTooltip mainW style delay1 delay2 innerW = mdo
  (e, ret) <- elClass' "div" "div-tooltip-wrapper" $ do
    ret' <- mainW
    let
      eMouseIn = domEvent Mouseenter e
      eMouseOut = domEvent Mouseleave e
    eShow <- delay delay1 eMouseIn
    eHide <- delay delay2 eMouseOut
    dAttrs <- holdDyn hideAttrs $ leftmost
      [ showAttrs <$ eShow
      , hideAttrs <$ eHide
      ]
    elDynAttr "div" dAttrs innerW
    return ret'
  return ret
  where
    constAttrs = "class" =: "div-tooltip top"
    showAttrs = constAttrs <> "style" =: ("display:inline-block;" <> style)
    hideAttrs = constAttrs <> "style" =: ("display:none;" <> style)