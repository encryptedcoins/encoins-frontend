{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.ImportWindow
  ( importWindow
  , importFileWindow
  , exportWindow
  ) where

import           Control.Monad                   (void, (<=<))
import           Data.Aeson                      (decode)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import           Data.Text.Encoding              (encodeUtf8)
import           GHCJS.DOM.EventM                (on)
import           GHCJS.DOM.FileReader            (getResult, load,
                                                  newFileReader, readAsText)
import           GHCJS.DOM.Types                 (File, fromJSVal, liftJSM,
                                                  toJSVal)
import           JS.Website                      (saveTextFile)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           Backend.Protocol.Utility        (hexToSecret)
import           Backend.Utility                 (switchHoldDyn)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Utils            (toJsonText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)

importWindow :: MonadWidget t m => Event t () -> m (Event t (Maybe Secret))
importWindow eImportOpen = mdo
    eImportClose <- dialogWindow True eImportOpen (void eImportClose) "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px)" "Import a New Coin" $ mdo
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "All known coins are saved on the device. Enter the minting key to import a new coin:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
              <> "style" =: "width: min(100%, 810px); margin-bottom: 15px" <> "placeholder" =: "0a00f07d1431910315c05aa5204c5e8f9e0c6..."))
      t <- inputElement conf
      let d    = hexToSecret <$> _inputElement_value t
      eImportClose <- btn "button-switching inverted flex-center" "" $ text "Ok"
      return $ current d `tag` eImportClose
    return eImportClose

importFileWindow :: MonadWidget t m => Event t () -> m (Event t [Secret])
importFileWindow eImportOpen = mdo
    eImportClose <- dialogWindow True eImportOpen (void eImportClose) "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px)" "Import New Coins" $ mdo
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Choose a file to import coins:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "file"
              <> "style" =: "width: min(100%, 788px); margin-bottom: 15px; box-sizing: content-box;"))
      dFiles <- _inputElement_files <$> inputElement conf
      emFileContent <- switchHoldDyn dFiles $ \case
        [file] -> readFileContent file
        _      -> pure never
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
    eClose <- dialogWindow True eOpen eClose "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px)" "Export Coins" $ mdo
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Enter file name:"
      let conf    = def { _inputElementConfig_setValue = pure ("" <$ eOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
              <> "style" =: "width: min(100%, 810px); margin-bottom: 15px" <> "placeholder" =: "coins.txt"))
      dFile <- value <$> inputElement conf
      eSave <- btn "button-switching inverted flex-center" "" $ text "Save"
      let dContent = toJsonText <$> dSecrets
      performEvent_ (uncurry saveTextFile <$> (current (zipDyn dFile dContent) `tag` eSave))
      return eSave
    blank
