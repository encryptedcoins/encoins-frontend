module ENCOINS.App.Widgets.PasswordWindow where

import           Control.Monad                   (void, (<=<))
import           Data.Aeson                      (encode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (toStrict)
import           Data.Char                       (isLower, isSymbol, isUpper)
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           ENCOINS.App.Widgets.Basic       (loadTextFromStorage, saveTextToStorage, loadAppData)
import           ENCOINS.Bulletproofs            (Secrets)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn)
import           JS.Website                      (saveJSON)

passwordSotrageKey :: Text
passwordSotrageKey = "password-hash"

newtype PasswordRaw = PasswordRaw { getPassRaw :: Text } deriving (Eq, Show)

newtype PasswordHash = PasswordHash { getPassHash :: Text } deriving (Eq, Show)

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

enterPasswordWindow :: MonadWidget t m => PasswordHash -> m (Event t (), Event t ())
enterPasswordWindow passHash = mdo
  dWindowIsOpen <- holdDyn True (False <$ eClose)
  ret@(eClose,_) <- elDynAttr "div" (fmap mkClass dWindowIsOpen) $ elAttr "div"
    ("class" =: "dialog-window" <> "style" =: "width:90%") $ do
      divClass "app-columns w-row" $ divClass "connect-title-div" $
        divClass "app-text-semibold" $ text "Welcome to Encoins"
      dPassOk <- divClass "app-columns w-row" $
        divClass "app-column w-col w-col-12" $ do
          ePb <- getPostBuild
          dmCurPass <- passwordInput "Enter your password:" False (pure Nothing) ePb
          return $ checkPass passHash <$> dmCurPass
      (eReset, eOk) <- elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
        eSave' <- btn "button-switching inverted flex-center"
          "display:inline-block;margin-left:5px;" $ text "Ok"
        eReset' <- btn "button-switching flex-center"
            "display:inline-block;margin-right:5px;" $ text "Reset password"
        return (eReset', eSave')
      widgetHold_ blank $ leftmost
        [ bool err blank <$> tagPromptlyDyn dPassOk eOk
        , blank <$ updated dPassOk ]
      return (void $ ffilter id (tagPromptlyDyn dPassOk eOk), eReset)
  return ret
  where
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect password"
    mkClass b = "class" =: "dialog-window-wrapper" <>
      bool ("style" =: "display: none") mempty b
    checkPass hash (Just raw) = rawToHash raw == hash
    checkPass _ _ = False

passwordSettingsWindow :: MonadWidget t m => Event t () -> m (Event t ())
passwordSettingsWindow eOpen = do
  emPass <- fmap (>>= checkPasswordHash) <$> performEvent
    (loadTextFromStorage passwordSotrageKey <$ eOpen)
  dmPass <- holdDyn Nothing emPass
  dynText $ maybe "NONE" getPassHash <$> dmPass
  eRes <- dialogWindow True eOpen never "width: 90%;" $ do
    ePassOk <- switchHold never <=< dyn $ dmPass <&> \case
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
    (eReset, eSave) <- elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
      eSave' <- btn (mkSaveBtnCls <$> dmPass <*> dPassOk <*> dmNewPass)
        "display:inline-block;margin-left:5px;" $ text "Save"
      eReset' <- switchHold never <=< dyn $ dmPass <&> \case
        Just _ -> btn "button-switching flex-center"
          "display:inline-block;margin-right:5px;" $ text "Reset password"
        Nothing -> pure never
      return (eReset', eSave')
    performEvent_ (saveJSON passwordSotrageKey . getPassHash . rawToHash <$>
      catMaybes (tagPromptlyDyn dmNewPass eSave))
    widgetHold_ blank $ leftmost
      [ elAttr "div" ("class" =: "app-columns w-row" <> "style" =:
        "display:flex;justify-content:center;") (text "Password saved!") <$ eSave
      , blank <$ eOpen ]
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

resetPasswordDialog :: MonadWidget t m => Event t () -> m (Event t ())
resetPasswordDialog eOpen = mdo
  (eOk, eCancel) <- dialogWindow True eOpen (leftmost [eOk,eCancel]) "width: 60%" $ do
    divClass "connect-title-div" $ divClass "app-text-semibold" $
        text "This action will clean the list of known coins! Are you sure?"
    elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
      btnOk <- btn "button-switching inverted flex-center"
        "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      btnCancel <- btn "button-switching flex-center"
        "width:30%;display:inline-block;margin-left:5px;" $ text "Cancel"
      return (btnOk, btnCancel)
  performEvent_ (saveTextToStorage passwordSotrageKey
    (getPassHash . rawToHash $ PasswordRaw "") <$ eOk)
  performEvent_ ((saveJSON "encoins" . decodeUtf8 . toStrict $ encode @Secrets []) <$ eOk)
  return eOk
