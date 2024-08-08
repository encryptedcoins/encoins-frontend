{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.ImportWindow
    ( exportWindow
    , importWindow
    ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.FileReader (getResult, load, newFileReader, readAsText)
import GHCJS.DOM.Types (File, fromJSVal, liftJSM, toJSVal)
import JS.Website (saveTextFile)
import Reflex.Dom
import Witherable (catMaybes)

import Common.Events
import Common.Protocol (hexToSecret)
import Common.Reflex.Extra (switchHoldDyn)
import Common.Utility (formatCoinTime, toJsonText)
import ENCOINS.Bulletproofs (Secret)
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Basic (btn, btnWithBlock)
import qualified I18n.App as I18n
import I18n.I18n (App)
import qualified I18n.I18n as I18n

importWindow ::
    (App t m) => Event t () -> m (Event t [Secret], Event t [Secret])
importWindow eImportOpen = mdo
    (s, ss) <- dialogWindow
        True
        eImportOpen
        eImportClose
        "app-ImportWindow"
        (I18n.AppTerm I18n.ImportWindowTitle)
        $ divClass "app-ImportWindow_Container"
        $ do
            emSecret <- importMintingKey eImportOpen
            eSecrets <- importCoinFiles eImportOpen
            pure (emSecret, eSecrets)
    let eImportClose = leftmost [() <$ s, () <$ ss]
    pure ((: []) <$> catMaybes s, ss)

importMintingKey ::
    (MonadWidget t m) =>
    Event t ()
    -> m (Event t (Maybe Secret))
importMintingKey eImportOpen = divClass "app-ImportKey_Container" $ mdo
    divClass "app-ImportWindow_SubTitle" $
        text "Enter the minting key to import a new coin:"
    let conf =
            def{_inputElementConfig_setValue = pure ("" <$ eImportOpen)}
                & ( initialAttributes
                        .~ ( "class" =: "app-ImportKey_Input"
                                <> "type" =: "text"
                                <> "placeholder" =: "0a00f07d1431910315c05aa5204c5e8f9e0c6..."
                           )
                  )
                & inputElementConfig_elementConfig
                . elementConfig_modifyAttributes
                .~ (("style" =:) . Just <$> updated dBorder)
    (eImportClose, dmSec, dBorder) <- divClass "app-ImportKey_Container_InputAndButton" $ do
        dText <- _inputElement_value <$> inputElement conf
        let dmSecret = hexToSecret <$> dText
        eClose <-
            divClass "app-ImportKey_ButtonContainer" $
                btnWithBlock "button-switching inverted flex-center" "" (isNothing <$> dmSecret) $
                    text "Ok"
        let dBorderLine = zipDynWith selectBorderColor dmSecret dText
        pure (eClose, dmSecret, dBorderLine)
    pure $ tagPromptlyDyn dmSec eImportClose

selectBorderColor :: Maybe Secret -> Text -> Text
selectBorderColor mSecret origInput =
    if T.null origInput
        then T.empty
        else maybe "border-color: #ff3e31;" (const "border-color: #00cb7a;") mSecret

importCoinFiles :: (MonadWidget t m) => Event t () -> m (Event t [Secret])
importCoinFiles eImportOpen = do
    (eImportClose, dInputFiles) <- divClass "app-ImportFile_Container" $ do
        divClass "app-ImportWindow_SubTitle" $ text "Choose a file to import coins:"
        let conf =
                def{_inputElementConfig_setValue = pure ("" <$ eImportOpen)}
                    & (initialAttributes .~ ("class" =: "app-ImportFile_Input" <> "type" =: "file"))
        divClass "app-ImportFile_Container_InputAndButton" $ do
            dFiles <- _inputElement_files <$> inputElement conf
            eClose <- divClass "app-ImportFile_ButtonContainer" $ do
                btn "button-switching inverted flex-center app-ImportFile_Button" "" $ text "Ok"
            pure (eClose, dFiles)

    emFileContent <- switchHoldDyn dInputFiles $ \case
        [file] -> readFileContent file
        _ -> pure never
    dContent <- holdDyn "" (catMaybes emFileContent)
    let parseContent = fromMaybe [] . decode . fromStrict . encodeUtf8
    let dResult = parseContent <$> dContent
    return (current dResult `tag` eImportClose)

readFileContent :: (MonadWidget t m) => File -> m (Event t (Maybe Text))
readFileContent file = do
    fileReader <- newFileReader
    readAsText fileReader (Just file) (Nothing @Text)
    wrapDomEvent fileReader (`on` load) . liftJSM $ do
        v <- getResult fileReader
        (fromJSVal <=< toJSVal) v

exportWindow ::
    (App t m) =>
    Event t ()
    -> Dynamic t [Secret]
    -> Dynamic t [Secret]
    -> m ()
exportWindow eOpen dSelectedSecrets dAllSecrets = mdo
    eClose <- dialogWindow
        True
        eOpen
        eClose
        "app-ExportWindow"
        (I18n.AppTerm I18n.ExportWindowTitle)
        $ mdo
            elAttr
                "div"
                ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between")
                $ text "Enter file name:"
            eTime <- performEvent ((formatCoinTime <$> liftIO getCurrentTime) <$ eOpen)
            logEvent "eTime" eTime
            let eDefaultValue = (\time -> "encoins" <> "-of-" <> time <> ".txt") <$> eTime
            logEvent "eDefaultValue" eDefaultValue
            let conf =
                    def{_inputElementConfig_setValue = Just eDefaultValue}
                        & ( initialAttributes
                                .~ ( "class" =: "coin-new-input w-input"
                                        <> "type" =: "text"
                                        <> "style" =: "width: min(100%, 810px); margin-bottom: 15px"
                                        <> "placeholder" =: "coins.txt"
                                   )
                          )
            dFile <- value <$> inputElement conf

            (eSaveSelected, eSaveAll) <- divClass "app-ExportWindow_ButtonContainer" $ do
                eSelected <-
                    btn "button-switching inverted flex-center app-ExportSelected_Button" "" $
                        text "Save Selected"
                eAll <-
                    btn "button-switching inverted flex-center app-ExportAll_Button" "" $
                        text "Save all"
                pure (eSelected, eAll)

            let dSelectedContent = toJsonText <$> dSelectedSecrets
            performEvent_
                ( uncurry saveTextFile
                    <$> (current (zipDyn dFile dSelectedContent) `tag` eSaveSelected)
                )
            let dAllContent = toJsonText <$> dAllSecrets
            performEvent_
                (uncurry saveTextFile <$> (current (zipDyn dFile dAllContent) `tag` eSaveAll))

            pure $ leftmost [eSaveSelected, eSaveAll]
    blank
