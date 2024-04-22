{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.ImportWindow
  ( importWindow
  , exportWindow
  ) where

import           Control.Monad                   ((<=<))
import           Control.Monad.IO.Class          (liftIO)
import           Data.Aeson                      (decode)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Time                       (getCurrentTime)
import           GHCJS.DOM.EventM                (on)
import           GHCJS.DOM.FileReader            (getResult, load,
                                                  newFileReader, readAsText)
import           GHCJS.DOM.Types                 (File, fromJSVal, liftJSM,
                                                  toJSVal)
import           JS.Website                      (saveTextFile)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           Backend.Protocol.Utility        (hexToSecret)
import           Backend.Utility                 (formatCoinTime, switchHoldDyn)
import           ENCOINS.Bulletproofs            (Secret)
import           ENCOINS.Common.Utils            (toJsonText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           ENCOINS.Common.Events

importWindow :: MonadWidget t m => Event t () -> m (Event t [Secret], Event t [Secret])
importWindow eImportOpen = mdo
    (s,ss) <- dialogWindow True eImportOpen eImportClose "app-ImportWindow" "Import new Encoins" $ mdo
      emSecret <- importMintingKey eImportOpen
      eSecrets <- importCoinFiles eImportOpen
      pure (emSecret, eSecrets)
    let eImportClose = leftmost [() <$ s, () <$ ss]
    pure ((:[]) <$> catMaybes s,ss)

importMintingKey :: MonadWidget t m
  => Event t ()
  -> m (Event t (Maybe Secret))
importMintingKey eImportOpen = mdo
  elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
      text "Enter the minting key to import a new coin:"
  let conf = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
          <> "style" =: "width: min(100%, 810px); margin-bottom: 15px" <> "placeholder" =: "0a00f07d1431910315c05aa5204c5e8f9e0c6..."))
  t <- inputElement conf
  let d    = hexToSecret <$> _inputElement_value t
  eImportClose <- divClass "app-importMintingKey_ButtonContainer" $ do
    btn "button-switching inverted flex-center" "" $ text "Ok"
  pure $ current d `tag` eImportClose

importCoinFiles :: MonadWidget t m => Event t () -> m (Event t [Secret])
importCoinFiles eImportOpen = mdo
  elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
      text "Choose a file to import coins:"
  let conf    = def { _inputElementConfig_setValue = pure ("" <$ eImportOpen) } & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "file"
          <> "style" =: "width: min(100%, 788px); margin-bottom: 15px; box-sizing: content-box;"))
  dFiles <- _inputElement_files <$> inputElement conf
  emFileContent <- switchHoldDyn dFiles $ \case
    [file] -> readFileContent file
    _      -> pure never
  dContent <- holdDyn "" (catMaybes emFileContent)
  let parseContent = fromMaybe [] . decode . fromStrict . encodeUtf8
  let dRes = parseContent <$> dContent
  eImportClose <- divClass "app-ImportFileWindow_ButtonContainer" $ do
    btn "button-switching inverted flex-center" "" $ text "Ok"
  return (current dRes `tag` eImportClose)

readFileContent :: MonadWidget t m => File -> m (Event t (Maybe Text))
readFileContent file = do
  fileReader <- newFileReader
  readAsText fileReader (Just file) (Nothing @Text)
  wrapDomEvent fileReader (`on` load) . liftJSM $ do
    v <- getResult fileReader
    (fromJSVal <=< toJSVal) v

exportWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t [Secret]
  -> Dynamic t [Secret]
  -> m ()
exportWindow eOpen dSelectedSecrets dAllSecrets = mdo
    eClose <- dialogWindow True eOpen eClose "app-ExportWindow" "Export Encoins" $ mdo
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between") $
          text "Enter file name:"
      eTime <- performEvent ((formatCoinTime <$> liftIO getCurrentTime) <$ eOpen)
      logEvent "eTime" eTime
      let eDefaultValue =  (\time -> "encoins" <> "-of-" <> time <> ".txt") <$> eTime
      logEvent "eDefaultValue" eDefaultValue
      let conf = def { _inputElementConfig_setValue = Just eDefaultValue}
            & (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "type" =: "text"
              <> "style" =: "width: min(100%, 810px); margin-bottom: 15px" <> "placeholder" =: "coins.txt"))
      dFile <- value <$> inputElement conf

      eSaveSelected <- divClass "app-ExportWindow_ButtonContainer" $ do
        btn "button-switching inverted flex-center" "" $ text "Save Selected"
      let dSelectedContent = toJsonText <$> dSelectedSecrets
      performEvent_ (uncurry saveTextFile <$> (current (zipDyn dFile dSelectedContent) `tag` eSaveSelected))

      eSaveAll <- divClass "app-ExportWindow_ButtonContainer" $ do
        btn "button-switching inverted flex-center" "" $ text "Save all"
      let dAllContent = toJsonText <$> dAllSecrets
      performEvent_ (uncurry saveTextFile <$> (current (zipDyn dFile dAllContent) `tag` eSaveAll))
      pure $ leftmost [eSaveSelected, eSaveAll]
    blank
