
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.CloudWindow where

import           Backend.Protocol.Types
import           Backend.Status                  (CloudStatusIcon (..))
import           Backend.Utility                 (space, switchHoldDyn)
import           ENCOINS.App.Widgets.Basic       (saveAppData_)
import           ENCOINS.App.Widgets.Cloud        (fetchAesKey, genAesKey)
import           ENCOINS.Common.Cache            (isCloudOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Advanced (copyEvent, dialogWindow,
                                                  withTooltip)
import           ENCOINS.Common.Widgets.Basic    (image)
import           JS.Website                      (copyText)

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Text                       (Text)
import           Reflex.Dom


cloudSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t CloudStatusIcon
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw), Dynamic t (Maybe AesKeyRaw))
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

      dmOldKey <- fetchAesKey mPass "first-load-of-aes-key" $ leftmost [() <$ eCloudChangeValDelayed, eOpen]
      -- logDyn "cloudSettingsWindow: dmOldKey" dmOldKey
      emKey <- switchHoldDyn dIsCloudOn $ \case
        False -> pure never
        True -> do
          dmNewKey <- genAesKey mPass dmOldKey $ leftmost [() <$ eCloudChangeValDelayed, eOpen]
          divClass "app-Cloud_AesKey_Title" $
            text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
          showKeyWidget dmNewKey
          divClass "app-Cloud_Restore_Title" $
            text "'RESTORE' button restores all unburned encoins that saved remotely and that your aes key is able to decrypt"
          restoreButton dmNewKey
          pure $ updated dmNewKey
      dmNewKey <- holdDyn Nothing emKey
      pure (dIsCloudOn, dmNewKey, dmOldKey)

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
  let dKey = maybe "Save key is not generated" getAesKeyRaw <$> dmKey
  let keyIcon = do
        e <- image "Key.svg" "inverted" "22px"
        void $ copyEvent e
        let eKey = tagPromptlyDyn dKey e
        performEvent_ (liftIO . copyText <$> eKey)
  divClass "app-Cloud_KeyContainer" $ do
    divClass "key-div" $ withTooltip keyIcon "app-CloudWindow_KeyTip" 0 0 $ do
      text "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    dynText dKey

-- restoreButton :: MonadWidget t m
--   => Dynamic t (Maybe AesKeyRaw)
--   -> m (Event t ())
-- restoreButton dmKey = do
  -- btnWithBlock "" "" (isNothing <$> dmKey) $ text "Restore"