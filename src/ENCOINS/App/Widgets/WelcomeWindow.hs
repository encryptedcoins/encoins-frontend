module ENCOINS.App.Widgets.WelcomeWindow (welcomeWindow) where

import           Data.Bool                     (bool)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic     (btnApp)

welcomeWindow :: MonadWidget t m => m ()
welcomeWindow = mdo
    dWelcomeIsOpen <- holdDyn True (False <$ eWelcomeClose)
    let mkClass b = "class" =: "div-app-fixed" <> bool ("style" =: "display: none") mempty b
    eWelcomeClose <- elDynAttr "div" (fmap mkClass dWelcomeIsOpen) $
        divClass "connect-div" $ mdo
            divClass "connect-title-div" $ do
                divClass "app-text-semibold" $ text "Welcome to ENCOINS Private Test!"
            btnApp "button-switching inverted" $ text "Ok" 
    blank