module ENCOINS.App.Widgets.ImportWindow (importWindow) where

import           Reflex.Dom

import           ENCOINS.App.Widgets.Coin        (hexToSecret)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)

importWindow :: MonadWidget t m => Event t () -> m (Event t (Maybe Secret))
importWindow eImportOpen = mdo
    dImportIsOpen <- holdDyn False $ leftmost [True <$ eImportOpen, False <$ eImportClose]
    eImportClose <- dialogWindow dImportIsOpen "max-width: 700px;" $ mdo
            divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Import New Coin"
            divClass "app-text-normal" $ text "Enter the minting key to import the coin:"
            let conf    = def & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
                    <> "placeholder" =: "Enter a minting key..."))
            t <- inputElement conf
            let d = hexToSecret <$> _inputElement_value t
            eImportClose <- btn "button-switching inverted flex-center" $ text "Ok"
            return $ current d `tag` eImportClose
    return eImportClose