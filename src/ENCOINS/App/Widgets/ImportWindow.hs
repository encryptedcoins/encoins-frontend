module ENCOINS.App.Widgets.ImportWindow (importWindow, importFileWindow, exportWindow) where

import           Control.Monad                   ((<=<), void)
import           Data.Aeson                      (encode, decode)
import           Data.ByteString.Lazy            (toStrict, fromStrict)
import           Data.Functor                    ((<&>))
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           GHCJS.DOM.EventM                (on)
import           GHCJS.DOM.FileReader            (newFileReader, readAsText, load, getResult)
import           GHCJS.DOM.Types                 (File, toJSVal, fromJSVal, liftJSM)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           ENCOINS.App.Widgets.Coin        (hexToSecret)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (logInfo, saveTextFile)
import           Widgets.Utils                   (toText)

importWindow :: MonadWidget t m => Event t () -> m (Event t (Maybe Secret))
importWindow eImportOpen = mdo
    eImportClose <- dialogWindow eImportOpen (void eImportClose) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
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
    eImportClose <- dialogWindow eImportOpen (void eImportClose) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Import New Coins"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Choose a file to import coins:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "file"
              <> "style" =: "width: 788px; margin-bottom: 15px; box-sizing: content-box;"))
      dFiles <- _inputElement_files <$> inputElement conf
      emFileContent <- switchHold never <=< dyn $ dFiles <&> \case
        [file] -> readFileContent file
        _ -> pure never
      performEvent_ $ logInfo . ("File content: " <>) . toText <$> emFileContent
      dContent <- holdDyn "" (catMaybes emFileContent)
      let dRes = parseContent <$> dContent
      eImportClose <- btn "button-switching inverted flex-center" "" $ text "Ok"
      return (current dRes `tag` eImportClose)
    return eImportClose
  where
    parseContent = fromMaybe [] . decode . fromStrict . encodeUtf8

readFileContent :: MonadWidget t m => File -> m (Event t (Maybe Text))
readFileContent file = do
  fileReader <- newFileReader
  readAsText fileReader (Just file) (Nothing @Text)
  wrapDomEvent fileReader (`on` load) . liftJSM $ do
    v <- getResult fileReader
    (fromJSVal <=< toJSVal) v

exportWindow :: MonadWidget t m => Event t () -> Dynamic t [Secret] -> m ()
exportWindow eOpen dSecrets = mdo
    eClose <- dialogWindow eOpen eClose "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Export Coins"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Enter file name:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
              <> "style" =: "width: 810px; margin-bottom: 15px" <> "placeholder" =: "coins.txt"))
      dFile <- value <$> inputElement conf
      eSave <- btn "button-switching inverted flex-center" "" $ text "Save"
      let dContent = decodeUtf8 . toStrict . encode <$> dSecrets
      performEvent_ (uncurry saveTextFile <$> (current (zipDyn dFile dContent) `tag` eSave))
      return eSave
    blank
