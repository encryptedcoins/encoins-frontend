module ENCOINS.App.Widgets.ImportWindow (importWindow) where

import           Reflex.Dom

import           ENCOINS.App.Widgets.Coin        (hexToSecret)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)

importWindow :: MonadWidget t m => Event t () -> m (Event t (Maybe Secret))
importWindow eImportOpen = mdo
    dImportIsOpen <- holdDyn False $ leftmost [True <$ eImportOpen, False <$ eImportClose]
    eImportClose <- dialogWindow dImportIsOpen "width: 900px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
            divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Import a New Coin"
            elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
                text "All known coins are saved on the device. Enter the minting key to import a new coin:"
            let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
                    <> "style" =: "width: 760px; margin-bottom: 15px" <> "placeholder" =: "0a00f07d1431910315c05aa5204c5e8f9e0c6..."))
            t <- inputElement conf
            let d    = hexToSecret <$> _inputElement_value t
            eImportClose <- btn "button-switching inverted flex-center" "" $ text "Ok"
            return $ current d `tag` eImportClose
    return eImportClose