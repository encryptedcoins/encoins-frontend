module ENCOINS.App.Widgets.PasswordWindow (passwordSettingsWindow, checkPasswordHash) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Bool                       (bool)
import           Data.Char                       (isLower, isSymbol, isUpper)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           ENCOINS.App.Widgets.Basic       (loadTextFromStorage, saveTextToStorage)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)

newtype PasswordRaw = PasswordRaw { getPassRaw :: Text } deriving (Eq, Show)

newtype PasswordHash = PasswordHash { getPassHash :: Text } deriving (Eq, Show, ToJSON, FromJSON)

checkPasswordHash :: Text -> Maybe PasswordHash
checkPasswordHash txt
  | rawToHash (PasswordRaw "") == (PasswordHash txt) = Nothing
  | otherwise = Just (PasswordHash txt)

rawToHash :: PasswordRaw -> PasswordHash
rawToHash = PasswordHash . getPassRaw -- FIXME

validatePassword :: Text -> Either Text PasswordRaw
validatePassword txt
  | T.length txt < 10 = Left "Password must be at least 10 characters long"
  | not (T.any isUpper txt) = Left "Password must contain at least one upper-case letter"
  | not (T.any isLower txt) = Left "Password must contain at least one lower-case letter"
  | not (T.any isSymbol txt) = Left "Password must contain at least one special character"
  | otherwise = Right (PasswordRaw txt)

passwordSettingsWindow :: MonadWidget t m => Event t () -> m (Event t ())
passwordSettingsWindow eOpen = mdo
  mPass <- fmap PasswordHash <$> loadTextFromStorage "password-hash"
  eRes <- dialogWindow True eOpen eRes "width: 60%;" $ do
    ePassOk <- case mPass of
      Just passHash -> divClass "app-columns w-row" $ divClass "app-column w-col w-col-12" $ do
        dmCurPass <- passwordInput "Current password:" False (pure Nothing) eOpen
        let dCheckedPass = checkPass passHash <$> dmCurPass
        dyn_ $ mkErr <$> dCheckedPass <*> dmCurPass
        return (ffilter id $ updated dCheckedPass)
      Nothing -> pure never
    dmNewPass <- divClass "app-columns w-row" $ mdo
      dmPass1 <- divClass "app-column w-col w-col-6" $ do
        passwordInput "Enter password:" False (pure Nothing) eOpen
      dmPass2 <- divClass "app-column w-col w-col-6" $ do
        passwordInput "Repeat password:" True dmPass1 eOpen
      return dmPass2
    dPassOk <- holdDyn False ePassOk
    elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $
      do
        eReset <- case mPass of
          Just _ -> btn "button-switching inverted flex-center"
            "width:10%;display:inline-block;margin-right:5px;" $ text "Reset"
          Nothing -> pure never
        eSave <- btn (mkSaveBtnCls mPass <$> dPassOk <*> dmNewPass)
            "width:10%;display:inline-block;margin-left:5px;" $ text "Save"
        performEvent_ (saveTextToStorage "password-hash" . getPassHash . rawToHash <$>
          catMaybes (tagPromptlyDyn dmNewPass eSave))
        widgetHold_ blank (text "Password saved!" <$ eSave)
        return eReset
  return eRes
  where
    mkErr _ Nothing = blank
    mkErr _ (Just (PasswordRaw "")) = blank
    mkErr c _ = bool (errDiv "Incorrect password") blank c
    checkPass hash (Just raw) = rawToHash raw == hash
    checkPass _ _ = False
    cls = "button-switching inverted flex-center"
    mkSaveBtnCls Nothing _ (Just _) = cls
    mkSaveBtnCls (Just _) True (Just _) = cls
    mkSaveBtnCls _ _ _ = cls <> " button-disabled"

passwordInput :: MonadWidget t m =>
  Text -> Bool -> Dynamic t (Maybe PasswordRaw) ->
  Event t () -> m (Dynamic t (Maybe PasswordRaw))
passwordInput txt rep dmPass eOpen = mdo
  dShowPass <- toggle False (domEvent Click eye)
  appTextLeft txt
  inp <- inputElement $ conf $ bool "password" "text" <$> updated dShowPass
  (eye,_) <- elDynAttr' "i" (mkEyeAttr <$> dShowPass) blank
  let deVal = validatePassword <$> value inp
  dyn_ $ mkError <$> (value inp) <*> deVal <*> dmPass
  return (zipDynWith mkRes deVal dmPass)
  where
    mkRes (Right p1) (Just p2) = if rep
      then if p1 == p2
        then Just p1
        else Nothing
      else Just p1
    mkRes ep1 _ = either (const Nothing) Just ep1
    mkError "" _ _ = blank
    mkError _ (Right p1) (Just p2) = if rep
      then if p1 == p2
        then blank
        else errDiv "Password doesn't match"
      else blank
    mkError _ (Right _) Nothing = if rep
      then errDiv "Password doesn't match"
      else blank
    mkError _ (Left err) _ = if rep
      then errDiv "Password doesn't match"
      else errDiv err
    mkEyeAttr showPass = "style" =: "cursor:pointer;display: inline-block;margin-left: -30px;"
      <> "class" =: ("far " <> bool "fa-eye" "fa-eye-slash" showPass)
    appTextLeft = elAttr "div" ("class" =: "app-text-normal" <>
      "style" =: "justify-content: left;") . text
    conf eType = def
      & initialAttributes .~ ("class" =: "w-input" <> "type" =: "password" <>
        "style" =: "display: inline-block;")
      & inputElementConfig_initialValue .~ ""
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~
        (("type" =:) . Just <$> eType)
      & inputElementConfig_setValue .~ ("" <$ eOpen)

errDiv :: MonadWidget t m => Text -> m ()
errDiv = elAttr "div" ("class" =: "w-file-upload-error w-file-upload-error-msg"
  <> "style" =: "margin-top: 0px;margin-bottom: 10px;") . text
