{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.CloudWindow where

import Backend.Protocol.Types
import Backend.Status (CloudIconStatus (..))
import Backend.Wallet (WalletName (..))
import Common.Events
import Common.Reflex.Dom.Extra (textLocale)
import Common.Reflex.Extra (switchHoldDyn)
import Common.Utility (space)
import ENCOINS.App.Widgets.Cloud (fetchAesKey, genAesKey, makeSignedKey)
import ENCOINS.Common.Cache
    ( aesKey
    , isCloudOn
    , removeCacheKey
    , saveAppData
    , saveAppData_
    )
import ENCOINS.Common.Widgets.Advanced
    ( dialogWindow
    , viewCopyButton
    , withTooltip
    )
import ENCOINS.Common.Widgets.Basic
    ( br
    , btn
    , btnWithBlock
    , btnWithOverOutBlock
    , image
    )
import qualified I18n.App as I18n
import qualified I18n.Common as I18n
import I18n.I18n (App)
import qualified I18n.I18n as I18n
import JS.Website (copyText)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Align (align)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import Text.Hex (decodeHex)

cloudSettingsWindow ::
    (App t m) =>
    Maybe PasswordRaw
    -> Dynamic t WalletName
    -> Dynamic t Bool
    -> Dynamic t CloudIconStatus
    -> Event t ()
    -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw), Event t ())
cloudSettingsWindow mPass dWalletName cloudCacheFlag dCloudStatus eOpen = mdo
    (dCloudOn, dmKey, eCloseByRestore) <- dialogWindow
        True
        eOpen
        eCloseByRestore
        "app-Cloud_Window"
        (I18n.AppTerm I18n.CloudWindowTitle)
        $ do
            (dIsCloudOn, eCloudChange) <- cloudCheckbox cloudCacheFlag
            cloudStatusIcon dCloudStatus dIsCloudOn

            let eCloudChangeVal = ffilter id $ tagPromptlyDyn dIsCloudOn eCloudChange
            eCloudChangeValDelayed <- delay 0.1 eCloudChangeVal

            (emNewKey, eRestore) <- fmap fanThese $ switchHoldDyn dIsCloudOn $ \case
                False -> pure never
                True -> do
                    let eFirstKeyLoad = leftmost [() <$ eCloudChangeValDelayed, eOpen]
                    dmNewKey <- cloudKeyWidget mPass dWalletName eFirstKeyLoad
                    divClass "app-Cloud_Restore_Title" $
                        text "Restore all unburned encoins from cloud with your current key"
                    eRestore <- viewRestoreButton dmNewKey
                    pure $ align (updated dmNewKey) eRestore
            dmNewKey <- holdDyn Nothing emNewKey
            pure (dIsCloudOn, dmNewKey, eRestore)
    pure (dCloudOn, dmKey, eCloseByRestore)

cloudCheckbox ::
    (MonadWidget t m) =>
    Dynamic t Bool
    -> m (Dynamic t Bool, Event t Bool)
cloudCheckbox cloudCacheFlag = do
    (dIsChecked, eCloudChange) <-
        viewCheckbox (updated cloudCacheFlag) "app-Cloud_CheckboxToggle"
    saveAppData_ Nothing isCloudOn $ updated dIsChecked
    pure (dIsChecked, eCloudChange)

cloudStatusIcon ::
    (MonadWidget t m) =>
    Dynamic t CloudIconStatus
    -> Dynamic t Bool
    -> m ()
cloudStatusIcon dCloudStatus dIsSave = do
    divClass "app-Cloud_Status_Title" $
        text "Cloud synchronization status"
    divClass "app-Cloud_StatusText" $
        dynText $
            zipDynWith selectSaveStatusNote dCloudStatus dIsSave

selectSaveStatusNote :: CloudIconStatus -> Bool -> Text
selectSaveStatusNote status isCloud =
    let t = case (status, isCloud) of
            (_, False) -> "is turned off"
            (NoTokens, _) -> "is impossible. There are not tokens in the local cache"
            (Saving, _) -> "is in progress..."
            (AllSaved, _) -> "is completed successfully."
            (FailedSave, _) -> "failed"
     in "The synchronization" <> space <> t

viewCheckbox ::
    (MonadWidget t m) =>
    Event t Bool
    -> Text
    -> m (Dynamic t Bool, Event t Bool)
viewCheckbox initial checkBoxClass = divClass "w-row app-Cloud_CheckboxContainer" $ do
    inp <-
        inputElement $
            def
                & initialAttributes
                .~ ( "class" =: checkBoxClass
                        <> "type" =: "checkbox"
                   )
                & inputElementConfig_setChecked
                .~ initial
    divClass "app-Save_CheckboxDescription" $ text "Save encoins on cloud"
    pure (_inputElement_checked inp, _inputElement_checkedChange inp)

showKeyWidget ::
    (MonadWidget t m) =>
    Dynamic t (Maybe AesKeyRaw)
    -> m ()
showKeyWidget dmKey = do
    let dKey = maybe "Cloud key is absent" getAesKeyRaw <$> dmKey
    let keyIcon = do
            void $ image "info-black.svg" "app-Cloud_IconPopup" ""
    let copyIcon = do
            e <- viewCopyButton
            let eKey = tagPromptlyDyn dKey e
            performEvent_ (liftIO . copyText <$> eKey)
    divClass "app-Cloud_KeyContainer" $ do
        copyIcon
        withTooltip keyIcon "app-CloudWindow_KeyTip" 0 0 $ do
            text
                "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
        dynText dKey

viewRestoreButton ::
    (MonadWidget t m) =>
    Dynamic t (Maybe AesKeyRaw)
    -> m (Event t ())
viewRestoreButton dmKey =
    divClass "app-Cloud_Restore_ButtonContainer" $
        btnWithBlock "button-switching inverted flex-center" "" (isNothing <$> dmKey) $
            text "Restore"

cloudKeyWidget ::
    (App t m) =>
    Maybe PasswordRaw
    -> Dynamic t WalletName
    -> Event t ()
    -> m (Dynamic t (Maybe AesKeyRaw))
cloudKeyWidget mPass dWalletName eFirstLoadKey = mdo
    divClass "app-Cloud_AesKey_Title" $
        text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
    eLoadKey <-
        delay 0.05 $
            leftmost [eFirstLoadKey, eKeyRemoved, eKeyGenerated, eUserKeySaved, eSignedKey]
    dmKey <- fetchAesKey mPass "cloudKeyWidget-fetchAesKey" eLoadKey
    showKeyWidget dmKey

    let dmCorrectKey = checkUserKeyValid <$> dInputCloudKey
    let dBorderLine = zipDynWith selectBorderColor dmKey dmCorrectKey
    dInputCloudKey <- viewInputCloudKey dBorderLine eFirstLoadKey
    let eKeyInputByUser = attachPromptlyDynWithMaybe const dmCorrectKey eEnter
    eUserKeySaved <- saveAppData mPass aesKey eKeyInputByUser

    eKeyGenerated <- genAesKey mPass dmKey eGenerate

    eSignedKey <- makeSignedKey mPass dWalletName eGetSignedKey

    let dBlockEnter =
            zipDynWith
                (\mUserKey mCacheKey -> isNothing mUserKey || isJust mCacheKey)
                dmCorrectKey
                dmKey
    ( (eEnterOver, eEnterOut, eEnter)
        , (eGenOver, eGenOut, eGenerate)
        , (eSignOver, eSignOut, eGetSignedKey)
        , (eDelOver, eDelOut, eDelete)
        ) <- divClass "app-Cloud_Key_ButtonContainer" $ do
        eEnt <-
            btnWithOverOutBlock
                "button-switching inverted flex-center"
                ""
                dBlockEnter
                (text "Enter")
        eGen <-
            btnWithOverOutBlock
                "button-switching inverted flex-center"
                ""
                (isJust <$> dmKey)
                (text "Generate")
        eSign <-
            btnWithOverOutBlock
                "button-switching inverted flex-center"
                ""
                (zipDynWith (\mKey name -> isJust mKey || name == None) dmKey dWalletName)
                (text "SignKey")
        eDel <-
            btnWithOverOutBlock
                "button-switching inverted flex-center"
                ""
                (isNothing <$> dmKey)
                (text "Delete")
        pure (eEnt, eGen, eSign, eDel)
    eKeyRemoved <- deleteKeyDialog eDelete
    let eMouseOutButton = leftmost [eEnterOut, eGenOut, eSignOut, eDelOut]
    dButtonDescription <-
        holdDyn "To see more details, hover over the active button." $
            leftmost
                [ "Button 'Enter' confirmes manually input key." <$ eEnterOver
                , "Button 'Generate' generates random cloud key." <$ eGenOver
                , "Button 'SignKey' makes key basing on the sign of connected wallet."
                    <$ eSignOver
                , "Button 'Delete' removes currently set key." <$ eDelOver
                , "To see more details, hover over the active button." <$ eMouseOutButton
                ]
    divClass "app-Cloud_ButtonDescription" $ dynText dButtonDescription
    pure dmKey

viewInputCloudKey ::
    (MonadWidget t m) =>
    Dynamic t Text
    -> Event t ()
    -> m (Dynamic t Text)
viewInputCloudKey dBorder eOpen = divClass "w-row" $ do
    inp <-
        inputElement $
            def
                & initialAttributes
                .~ ( "class" =: "w-input"
                        <> "style" =: "display: inline-block;"
                        <> "placeholder" =: "cloud key should be exactly 64 hexadecimal digits"
                   )
                & inputElementConfig_setValue
                .~ ("" <$ eOpen)
                & inputElementConfig_elementConfig
                . elementConfig_modifyAttributes
                .~ (("style" =:) . Just <$> updated dBorder)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

checkUserKeyValid :: Text -> Maybe AesKeyRaw
checkUserKeyValid key =
    if T.length key == 64 && isJust (decodeHex key)
        then Just $ MkAesKeyRaw key
        else Nothing

selectBorderColor :: Maybe AesKeyRaw -> Maybe AesKeyRaw -> Text
selectBorderColor mKey mCorrectKey = case (mKey, mCorrectKey) of
    (Just _, _) -> "display: inline-block;"
    (Nothing, Nothing) -> "display: inline-block; border-color: #ff3e31;"
    (Nothing, Just _) -> "display: inline-block; border-color: #00cb7a;"

deleteKeyDialog :: (App t m) => Event t () -> m (Event t ())
deleteKeyDialog eDelete = mdo
    (eOk, eCancel) <- dialogWindow
        True
        eDelete
        (leftmost [eOk, eCancel])
        "app-DeleteKeyWindow"
        (I18n.AppTerm I18n.CloudDeleteWindowTitle)
        $ do
            divClass "app-DeleteKey_Description" $ do
                textLocale I18n.CloudDeleteWindowContent
                br
                textLocale I18n.AreYouSure
            elAttr "div" ("class" =: "w-row app-DeleteKey_ButtonContainer") $ do
                btnOk <- btn "button-switching inverted flex-center" "" $ textLocale I18n.Delete
                btnCancel <- btn "button-switching flex-center" "" $ textLocale I18n.Cancel
                return (btnOk, btnCancel)
    eKeyRemoved <- removeCacheKey $ aesKey <$ eOk
    return eKeyRemoved
