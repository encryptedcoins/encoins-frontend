{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.PasswordWindow where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import           Data.Char                       (isAsciiLower, isAsciiUpper,
                                                  isDigit, isLower, isUpper,
                                                  ord)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           Backend.Protocol.Types          (PasswordHash (..),
                                                  PasswordRaw (..))
import           Backend.Utility                 (hashKeccak512, isHashOfRaw,
                                                  switchHoldDyn, toPasswordHash)
import           ENCOINS.App.Widgets.Basic       (saveAppData_)
import           ENCOINS.Common.Cache            (encoinsV3, passwordSotrageKey)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Events           (setFocusDelayOnEvent)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (br, btn, errDiv)
import           JS.App                          (loadCacheValue,
                                                  saveHashedTextToStorage)

validatePassword :: Text -> Either Text PasswordRaw
validatePassword txt
  | not $ T.all validPasswordChar txt = Left "Password must consist of \
    \uppercase and lowercase letters, numbers, and special characters"
  | T.length txt < 10 = Left "Password must be at least 10 characters long"
  | not (T.any isUpper txt) = Left "Password must contain at least one upper-case letter"
  | not (T.any isLower txt) = Left "Password must contain at least one lower-case letter"
  | not (T.any isSpecial txt) = Left "Password must contain at least one special character"
  | otherwise = Right (PasswordRaw txt)

-- Uppercase and lowercase letters, numbers, and special characters from the
-- ASCII character set. (i.e. everything from the ASCII set except the control characters)
validPasswordChar :: Char -> Bool
validPasswordChar c = ord c >= 32 && ord c <= 126

isSpecial :: Char -> Bool
isSpecial c = validPasswordChar c &&
  not (isAsciiUpper c || isAsciiLower c || isDigit c)

enterPasswordWindow :: MonadWidget t m => PasswordHash -> Event t () ->
  m (Event t PasswordRaw, Event t ())
enterPasswordWindow passHash eResetOk = mdo
  dWindowIsOpen <- holdDyn True (False <$ leftmost [void eClose,eResetOk])
  let windowStyle = "width: min(90%, 750px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px)"
  ret@(eClose,_) <- elDynAttr "div" (fmap mkClass dWindowIsOpen) $ elAttr "div"
    ("class" =: "dialog-window" <> "style" =: windowStyle) $ do
      divClass "app-columns w-row" $ divClass "connect-title-div" $
        divClass "app-text-semibold" $ text "Password for the cache of Encoins app"
      dPassOk <- divClass "app-columns w-row" $
        divClass "w-col w-col-12" $ do
          ePb <- getPostBuild
          dmCurPass <- passwordInput "Enter password:" False True (pure Nothing) ePb
          pure $ checkPass passHash <$> dmCurPass
      (eClean, eOk) <- elAttr "div" ("class" =: "app-columns w-row app-EnterPassword_ButtonContainer") $ do
        eSave' <- btn
          "button-switching inverted flex-center"
          ""
          $ text "Ok"
        eClean' <- btn
          "button-switching flex-center"
          ""
          $ text "Clean cache"
        return (eClean', eSave')
      widgetHold_ blank $ leftmost
        [ maybe err (const blank) <$> tagPromptlyDyn dPassOk eOk
        , blank <$ updated dPassOk ]
      return (catMaybes $ tagPromptlyDyn dPassOk eOk, eClean)
  return ret
  where
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect password"
    mkClass b = "class" =: "dialog-window-wrapper" <>
      bool ("style" =: "display: none") mempty b
    checkPass hash mRaw = do
      raw <- mRaw
      if isHashOfRaw (getPassHash hash) (getPassRaw raw)
        then Just raw
        else Nothing

passwordSettingsWindow :: MonadWidget t m
  => Event t ()
  -> m (Event t (Maybe PasswordRaw), Event t ())
passwordSettingsWindow eOpen = do
  ePassHash <- performEvent (loadCacheValue passwordSotrageKey <$ eOpen)
  let emPassHash = toPasswordHash <$> ePassHash
  dmPassHash <- holdDyn Nothing emPassHash
  dialogWindow True eOpen never "app-PasswordSettingsWindow" "Protect cache of Encoins app" $ do
    ePassOk <- switchHoldDyn dmPassHash $ \case
      Just passHash -> divClass "app-columns w-row" $ divClass "w-col w-col-12" $ do
        dmCurPass <- passwordInput "Current password:" False True (pure Nothing) eOpen
        let dCheckedPass = checkPass passHash <$> dmCurPass
        dyn_ $ mkErr <$> dCheckedPass <*> dmCurPass
        return (ffilter id $ updated dCheckedPass)
      Nothing -> pure never
    dmNewPass <- divClass "app-PasswordProtect_Window" $ mdo
      dmPass1 <- divClass "w-col" $ do
        passwordInput "Enter password:" False True (pure Nothing) eOpen
      dmPass2 <- divClass "w-col" $ do
        passwordInput "Repeat password:" True False dmPass1 eOpen
      return dmPass2
    dPassOk <- holdDyn False ePassOk
    (eReset, eClear, eSave) <- elAttr "div" ("class" =: "app-columns w-row app-PasswordSetting_ButtonContainer") $ do
      eSave' <- btn (mkSaveBtnCls <$> dmPassHash <*> dPassOk <*> dmNewPass) "" $ text "Save"
      eReset' <- switchHoldDyn dmPassHash $ \case
        Just _  -> btn (mkClearBtnCls <$> dPassOk) "white-space: nowrap;" $ text "Reset password"
        Nothing -> pure never
      eClear' <- switchHoldDyn dmPassHash $ \case
        Just _  -> btn "button-switching flex-center" "white-space: nowrap;" $ text "Clean cache"
        Nothing -> pure never
      return (eReset', eClear', eSave')
    let eNewPass = catMaybes (tagPromptlyDyn dmNewPass eSave)
    performEvent_ (saveHashedTextToStorage passwordSotrageKey (hashKeccak512 "") <$ eReset)
    performEvent_ (saveHashedTextToStorage passwordSotrageKey . hashKeccak512 . getPassRaw <$>
      eNewPass)
    widgetHold_ blank $ leftmost
      [ elAttr "div" ("class" =: "app-columns w-row" <> "style" =:
        "display:flex;justify-content:center;") (text "Password saved!") <$ eNewPass
      , elAttr "div" ("class" =: "app-columns w-row" <> "style" =:
        "display:flex;justify-content:center;") (text "Password cleared!") <$ eReset
      , blank <$ eOpen ]
    return (leftmost [Just <$> eNewPass, Nothing <$ eReset], eClear)
  where
    mkErr _ Nothing                 = blank
    mkErr _ (Just (PasswordRaw "")) = blank
    mkErr c _                       = bool (errDiv "Incorrect password") blank c
    checkPass hash (Just raw) = isHashOfRaw (getPassHash hash) (getPassRaw raw)
    checkPass _ Nothing       = False

    cls = "button-switching inverted flex-center"
    mkSaveBtnCls Nothing _ (Just _)     = cls
    mkSaveBtnCls (Just _) True (Just _) = cls
    mkSaveBtnCls _ _ _                  = cls <> " button-disabled"
    mkClearBtnCls = (cls <>) . bool " button-disabled" ""

passwordInput :: MonadWidget t m
  => Text
  -> Bool
  -> Bool
  -> Dynamic t (Maybe PasswordRaw)
  -> Event t ()
  -> m (Dynamic t (Maybe PasswordRaw))
passwordInput txt rep isFocus dmPass eOpen = mdo
  dShowPass <- toggle False (domEvent Click eye)
  appTextLeft txt
  inp <- inputElement $ conf $ bool "password" "text" <$> updated dShowPass
  if isFocus then setFocusDelayOnEvent inp eOpen else blank
  (eye,_) <- elDynAttr' "i" (mkEyeAttr <$> dShowPass) blank
  let deVal = validatePassword <$> value inp
  dyn_ $ mkError <$> value inp <*> deVal <*> dmPass
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
      & initialAttributes .~
        (  "class" =: "w-input"
        <> "type" =: "password"
        <> "style" =: "display: inline-block;"
        )
      & inputElementConfig_initialValue .~ ""
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~
        (("type" =:) . Just <$> eType)
      & inputElementConfig_setValue .~ ("" <$ eOpen)

cleanCacheDialog :: MonadWidget t m => Event t () -> m (Event t ())
cleanCacheDialog eOpen = mdo
  (eOk, eCancel) <- dialogWindow True eOpen (leftmost [eOk,eCancel]) "app-CleanCacheWindow" "Clean cache" $ do
    divClass "app-CleanCache_Description" $ do
        text "This action will reset password and clean cache (remove known coins)!"
        br
        text "Are you sure?"
    elAttr "div" ("class" =: "app-columns w-row app-CleanCache_ButtonContainer") $ do
      btnOk <- btn "button-switching inverted flex-center" "" $ text "Clean"
      btnCancel <- btn "button-switching flex-center" "" $ text "Cancel"
      return (btnOk, btnCancel)
  performEvent_ (saveHashedTextToStorage passwordSotrageKey (hashKeccak512 "") <$ eOk)
  saveAppData_ Nothing encoinsV3 $ ("" :: Text) <$ eOk
  return eOk
