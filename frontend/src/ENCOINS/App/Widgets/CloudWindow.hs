
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.CloudWindow where

import           Backend.Protocol.Types
import           Backend.Status                  (CloudStatusIcon (..))
import           Backend.Utility                 (space, switchHoldDyn)
import           ENCOINS.App.Widgets.Basic       (removeCacheKey, saveAppData,
                                                  saveAppData_)
import           ENCOINS.App.Widgets.Cloud       (fetchAesKey, genAesKey)
import           ENCOINS.Common.Cache            (aesKey, isCloudOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Advanced (copyEvent, dialogWindow,
                                                  withTooltip)
import           ENCOINS.Common.Widgets.Basic    (btn, btnWithBlock, br, image)
import           JS.Website                      (copyText)

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Align                      (align)
import           Data.Maybe                      (isJust, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex.Dom
import           Text.Hex                        (decodeHex)


cloudSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t CloudStatusIcon
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw), Event t ())
cloudSettingsWindow mPass cloudCacheFlag dCloudStatus eOpen = mdo
    (dCloudOn, dmKey, eCloseByRestore) <- dialogWindow
        True
        eOpen
        eCloseByRestore
        "app-Cloud_Window"
        "Encoins Cloud Backup" $ do
          (dIsCloudOn, eCloudChange) <- cloudCheckbox cloudCacheFlag
          cloudStatusIcon dCloudStatus dIsCloudOn

          let eCloudChangeVal = ffilter id $ tagPromptlyDyn dIsCloudOn eCloudChange
          eCloudChangeValDelayed <- delay 0.1 eCloudChangeVal

          (emNewKey, eRestore) <- fmap fanThese $ switchHoldDyn dIsCloudOn $ \case
            False -> pure never
            True -> do
              let eFirstKeyLoad = leftmost [() <$ eCloudChangeValDelayed, eOpen]
              dmNewKey <- cloudKeyWidget mPass eFirstKeyLoad
              divClass "app-Cloud_Restore_Title" $
                text "Restore all unburned encoins from cloud with your current key"
              eRestore <- restoreButton dmNewKey
              pure $ align (updated dmNewKey) eRestore
          dmNewKey <- holdDyn Nothing emNewKey
          pure (dIsCloudOn, dmNewKey, eRestore)
    pure (dCloudOn, dmKey, eCloseByRestore)

cloudCheckbox :: MonadWidget t m
  => Dynamic t Bool
  -> m (Dynamic t Bool, Event t Bool)
cloudCheckbox cloudCacheFlag = do
  (dIsChecked, eCloudChange) <- checkboxWidget (updated cloudCacheFlag) "app-Cloud_CheckboxToggle"
  saveAppData_ Nothing isCloudOn $ updated dIsChecked
  pure (dIsChecked, eCloudChange)

cloudStatusIcon :: MonadWidget t m
  => Dynamic t CloudStatusIcon
  -> Dynamic t Bool
  -> m ()
cloudStatusIcon dCloudStatus dIsSave = do
  divClass "app-Cloud_Status_Title" $
    text "Cloud synchronization status"
  divClass "app-Cloud_StatusText" $
    dynText $ zipDynWith selectSaveStatusNote dCloudStatus dIsSave

selectSaveStatusNote :: CloudStatusIcon -> Bool -> Text
selectSaveStatusNote status isCloud =
  let t = case (status, isCloud) of
        (_, False)      -> "is turned off"
        (NoTokens, _)   -> "is impossible. There are not tokens in the local cache"
        (Saving, _)     -> "is in progress..."
        (AllSaved, _)   -> "is completed successfully."
        (FailedSave, _) -> "failed"
  in "The synchronization" <> space <> t

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> m (Dynamic t Bool, Event t Bool)
checkboxWidget initial checkBoxClass = divClass "w-row app-Cloud_CheckboxContainer" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass <>
            "type" =: "checkbox"
          )
      & inputElementConfig_setChecked .~ initial
    divClass "app-Save_CheckboxDescription" $ text "Save encoins on cloud"
    pure (_inputElement_checked inp, _inputElement_checkedChange inp)

showKeyWidget :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m ()
showKeyWidget dmKey = do
  let dKey = maybe "Cloud key is absent" getAesKeyRaw <$> dmKey
  let keyIcon = do
        e <- image "info-popup.svg" "app-Cloud_IconPopup" ""
        void $ copyEvent e
        let eKey = tagPromptlyDyn dKey e
        performEvent_ (liftIO . copyText <$> eKey)
  divClass "app-Cloud_KeyContainer" $ do
    divClass "key-div" $ withTooltip keyIcon "app-CloudWindow_KeyTip" 0 0 $ do
      text "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    dynText dKey

restoreButton :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m (Event t ())
restoreButton dmKey = divClass "app-Cloud_Restore_ButtonContainer" $
  btnWithBlock "button-switching inverted flex-center" "" (isNothing <$> dmKey) $ text "Restore"

cloudKeyWidget :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t (Maybe AesKeyRaw))
cloudKeyWidget mPass eFirstLoadKey = mdo
  divClass "app-Cloud_AesKey_Title" $
    text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
  eLoadKey <- delay 0.05 $ leftmost [eFirstLoadKey, eKeyRemoved, eKeyGenerated, eUserKeySaved]
  dmKey <- fetchAesKey mPass "cloudKeyWidget-fetchAesKey" eLoadKey
  showKeyWidget dmKey

  let dmCorrectKey = checkUserKeyValid <$> dInputCloudKey
  let dBorderLine = zipDynWith selectBorderColor dmKey  dmCorrectKey
  dInputCloudKey <- inputCloudKeyWidget dBorderLine eFirstLoadKey
  let eKeyInputByUser = attachPromptlyDynWithMaybe const dmCorrectKey eEnter
  eUserKeySaved <- saveAppData mPass aesKey eKeyInputByUser

  eKeyGenerated <- genAesKey mPass dmKey eGenerate
  let dBlockEnter = zipDynWith
        (\mUserKey mCacheKey -> isNothing mUserKey || isJust mCacheKey)
        dmCorrectKey
        dmKey
  (eGenerate, eDelete, eEnter) <- divClass "app-Cloud_Key_ButtonContainer" $ do
    eGen <- btnWithBlock
      "button-switching inverted flex-center" "" (isJust <$> dmKey) (text "Generate")
    eDel <- btnWithBlock
      "button-switching inverted flex-center" "" (isNothing <$> dmKey) (text "Delete")
    eEnt <- btnWithBlock
      "button-switching inverted flex-center" "" dBlockEnter (text "Enter")
    pure (eGen, eDel, eEnt)
  eKeyRemoved <- deleteKeyDialog eDelete
  pure dmKey

inputCloudKeyWidget :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -> m (Dynamic t Text)
inputCloudKeyWidget dBorder eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "cloud key should be exactly 64 hexadecimal digits"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~
          (("style" =:) . Just <$> updated dBorder)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

checkUserKeyValid :: Text -> Maybe AesKeyRaw
checkUserKeyValid key = if T.length key == 64 && isJust (decodeHex key)
  then Just $ MkAesKeyRaw key
  else Nothing

selectBorderColor :: Maybe AesKeyRaw -> Maybe AesKeyRaw -> Text
selectBorderColor mKey mCorrectKey = case (mKey, mCorrectKey) of
  (Just _, _)        -> "display: inline-block;"
  (Nothing, Nothing) -> "display: inline-block; border-color: #ff3e31;"
  (Nothing, Just _)  -> "display: inline-block; border-color: #00cb7a;"

deleteKeyDialog :: MonadWidget t m => Event t () -> m (Event t ())
deleteKeyDialog eDelete = mdo
  (eOk, eCancel) <- dialogWindow
    True
    eDelete
    (leftmost [eOk,eCancel])
    "app-DeleteKeyWindow"
    "Delete Cloud Key" $ do
      divClass "app-DeleteKey_Description" $ do
          text "This action will remove cloud key from the cache! If you won't remember the key you can't recover encoins from remote server!"
          br
          text "Are you sure?"
      elAttr "div" ("class" =: "app-columns w-row app-DeleteKey_ButtonContainer") $ do
        btnOk <- btn "button-switching inverted flex-center" "" $ text "Delete"
        btnCancel <- btn "button-switching flex-center" "" $ text "Cancel"
        return (btnOk, btnCancel)
  eKeyRemoved <- removeCacheKey $ aesKey <$ eOk
  return eKeyRemoved
