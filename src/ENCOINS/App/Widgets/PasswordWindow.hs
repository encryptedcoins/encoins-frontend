module ENCOINS.App.Widgets.PasswordWindow (passwordWindow) where

import           Control.Monad                   (void)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic       (loadAppData)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (saveJSON)
import           Widgets.Utils                   (toText)

passwordWindow :: MonadWidget t m => Event t () -> m ()
passwordWindow eOpen = mdo
  eClose <- dialogWindow True eOpen eClose "" $ do
    text "TBD"
    btn "button-switching inverted flex-center" "" $ text "Ok"
  blank
