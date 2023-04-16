module ENCOINS.App.Widgets.ImportWindow (importWindow, importFileWindow) where

import           Control.Monad                   ((<=<))
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           GHCJS.DOM.EventM                (on)
import           GHCJS.DOM.FileReader            (newFileReader, readAsText, load, getResult)
import           GHCJS.DOM.Types                 (File, toJSVal, fromJSVal, liftJSM)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           ENCOINS.App.Widgets.Coin        (hexToSecret)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (logInfo)
import           Widgets.Utils                   (toText)

importWindow :: MonadWidget t m => Event t () -> m (Event t (Maybe Secret))
importWindow eImportOpen = mdo
    dImportIsOpen <- holdDyn False $ leftmost [True <$ eImportOpen, False <$ eImportClose]
    eImportClose <- dialogWindow dImportIsOpen "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Import a New Coin"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "All known coins are saved on the device. Enter the minting key to import a new coin:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
              <> "style" =: "width: 810px; margin-bottom: 15px" <> "placeholder" =: "0a00f07d1431910315c05aa5204c5e8f9e0c6..."))
      t <- inputElement conf
      let d    = hexToSecret <$> _inputElement_value t
      eImportClose <- btn "button-switching inverted flex-center" "" $ text "Ok"
      return $ current d `tag` eImportClose
    return eImportClose

importFileWindow :: MonadWidget t m => Event t () -> m (Event t [Secret])
importFileWindow eImportOpen = mdo
    dImportIsOpen <- holdDyn False $ leftmost [True <$ eImportOpen, False <$ eImportClose]
    eImportClose <- dialogWindow dImportIsOpen "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Import New Coins"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Choose a file to import coins:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "file"
              <> "style" =: "width: 810px; margin-bottom: 15px"))
      dFiles <- _inputElement_files <$> inputElement conf
      emFileContent <- switchHold never <=< dyn $ dFiles <&> \case
        [file] -> readFileContent file
        _ -> pure never
      performEvent_ $ logInfo . ("File content: " <>) . toText <$> emFileContent
      dContent <- holdDyn "" (catMaybes emFileContent)
      let dRes = mapMaybe hexToSecret . Text.lines <$> dContent
      eImportClose <- btn "button-switching inverted flex-center" "" $ text "Ok"
      return (current dRes `tag` eImportClose)
    return eImportClose

readFileContent :: MonadWidget t m => File -> m (Event t (Maybe Text))
readFileContent file = do
  fileReader <- newFileReader
  readAsText fileReader (Just file) (Nothing @Text)
  wrapDomEvent fileReader (`on` load) . liftJSM $ do
    v <- getResult fileReader
    (fromJSVal <=< toJSVal) v
