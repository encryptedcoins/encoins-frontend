
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
import           ENCOINS.Common.Widgets.Basic    (btnWithBlock, image)
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
cloudSettingsWindow mPass cloudCacheFlag dCloudStatus eOpen = do
  dialogWindow
    True
    eOpen
    never
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
            text "Restore all unburned encoins that saved remotely and that your aes key is able to decrypt"
          eRestore <- restoreButton dmNewKey
          pure $ align (updated dmNewKey) eRestore
      dmNewKey <- holdDyn Nothing emNewKey
      pure (dIsCloudOn, dmNewKey, eRestore)

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
        e <- image "Key.svg" "inverted" "22px"
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
  let eLoadKey = leftmost [eFirstLoadKey, eKeyRemoved, eKeyGenerated, eUserKeySaved]
  logEvent "cloudKeyWidget: eLoadKey" eLoadKey
  dmKey <- fetchAesKey mPass "cloudKeyWidget-fetchAesKey" eLoadKey
  logDyn "cloudKeyWidget: dmKey" dmKey
  showKeyWidget dmKey

  dInputCloudKey <- inputCloudKeyWidget eFirstLoadKey
  let dmCorrectKey = checkUserKeyValid <$> dInputCloudKey
  logDyn "cloudKeyWidget: dmCorrectKey" dmCorrectKey
  let eKeyInputByUser = attachPromptlyDynWithMaybe const dmCorrectKey eEnter
  logEvent "cloudKeyWidget: eKeyInputByUser" eKeyInputByUser
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
  logEvent "cloudKeyWidget: eGenerate" eGenerate
  -- TODO: add alert message
  eKeyRemoved <- removeCacheKey $ aesKey <$ eDelete
  logEvent "cloudKeyWidget: eKeyRemoved" eKeyRemoved
  logEvent "cloudKeyWidget: eEnter" eEnter

  pure dmKey

inputCloudKeyWidget :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Text)
inputCloudKeyWidget eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "cloud key should be exactly 64 hexadecimal digits"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

checkUserKeyValid :: Text -> Maybe AesKeyRaw
checkUserKeyValid key = if T.length key == 64 && isJust (decodeHex key)
  then Just $ MkAesKeyRaw key
  else Nothing
