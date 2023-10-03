module ENCOINS.App.Widgets.PasswordWindow where

import           Control.Monad                   (void, (<=<))
import           Data.Aeson                      (encode)
import           Data.Bool                       (bool)
import           Data.ByteString.Lazy            (toStrict)
import           Data.Char                       (isLower, isUpper, ord, isAsciiLower, isAsciiUpper, isDigit)
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8)
import           Reflex.Dom
import           Witherable                      (catMaybes)

import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn, errDiv, br)
import           JS.App                          (saveHashedTextToStorage, loadHashedPassword, checkPassword)
import           JS.Website                      (saveJSON)
import           ENCOINS.Common.Events           (setFocusDelayOnEvent)


passwordSotrageKey :: Text
passwordSotrageKey = "password-hash"

newtype PasswordRaw = PasswordRaw { getPassRaw :: Text } deriving (Eq, Show)

newtype PasswordHash = PasswordHash { getPassHash :: Text } deriving (Eq, Show)

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
  let windowStyle = "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px"
  ret@(eClose,_) <- elDynAttr "div" (fmap mkClass dWindowIsOpen) $ elAttr "div"
    ("class" =: "dialog-window" <> "style" =: windowStyle) $ do
      divClass "app-columns w-row" $ divClass "connect-title-div" $
        divClass "app-text-semibold" $ text "Password for the Encoins application cache in the browser"
      dPassOk <- divClass "app-columns w-row" $
        divClass "app-column w-col w-col-12" $ do
          ePb <- getPostBuild
          dmCurPass <- passwordInput "Enter your password:" False True (pure Nothing) ePb
          emCurPass <- performEvent $ checkPass passHash <$> updated dmCurPass
          holdDyn Nothing emCurPass
      (eReset, eOk) <- elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
        eSave' <- btn "button-switching inverted flex-center"
          "display:inline-block;margin-left:5px;" $ text "Ok"
        eReset' <- btn "button-switching flex-center"
            "display:inline-block;margin-right:5px;" $ text "Clean cache"
        return (eReset', eSave')
      widgetHold_ blank $ leftmost
        [ maybe err (const blank) <$> tagPromptlyDyn dPassOk eOk
        , blank <$ updated dPassOk ]
      return (catMaybes $ tagPromptlyDyn dPassOk eOk, eReset)
  return ret
  where
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect password"
    mkClass b = "class" =: "dialog-window-wrapper" <>
      bool ("style" =: "display: none") mempty b
    checkPass hash (Just raw) = do
      res <- checkPassword (getPassHash hash) (getPassRaw raw)
      return $ bool Nothing (Just raw) res
    checkPass _ _ = return Nothing

passwordSettingsWindow :: MonadWidget t m => Event t ()
  -> m (Event t (Maybe PasswordRaw), Event t ())
passwordSettingsWindow eOpen = do
  emPassHash <- fmap (fmap PasswordHash) <$> performEvent (loadHashedPassword passwordSotrageKey <$ eOpen)
  dmPassHash <- holdDyn Nothing emPassHash
  let windowStyle = "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px"
  dialogWindow True eOpen never windowStyle "Protecting the Encoins application cache in the browser" $ do
    ePassOk <- switchHold never <=< dyn $ dmPassHash <&> \case
      Just passHash -> divClass "app-columns w-row" $ divClass "app-column w-col w-col-12" $ do
        dmCurPass <- passwordInput "Current password:" False True (pure Nothing) eOpen
        eCheckedPass <- performEvent $ checkPass passHash <$> updated dmCurPass
        dCheckedPass <- holdDyn False eCheckedPass
        dyn_ $ mkErr <$> dCheckedPass <*> dmCurPass
        return (ffilter id $ updated dCheckedPass)
      Nothing -> pure never
    dmNewPass <- divClass "app-columns w-row" $ mdo
      dmPass1 <- divClass "app-column w-col w-col-6" $ do
        passwordInput "Enter password:" False True (pure Nothing) eOpen
      dmPass2 <- divClass "app-column w-col w-col-6" $ do
        passwordInput "Repeat password:" True False dmPass1 eOpen
      return dmPass2
    dPassOk <- holdDyn False ePassOk
    (eReset, eClear, eSave) <- elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
      eSave' <- btn (mkSaveBtnCls <$> dmPassHash <*> dPassOk <*> dmNewPass)
        "display:inline-block;margin-left:5px;" $ text "Save"
      eClear' <- switchHold never <=< dyn $ dmPassHash <&> \case
        Just _ -> btn (mkClearBtnCls <$> dPassOk)
          "display:inline-block;margin-left:5px;" $ text "Reset password"
        Nothing -> pure never
      eReset' <- switchHold never <=< dyn $ dmPassHash <&> \case
        Just _ -> btn "button-switching flex-center"
          "display:inline-block;margin-right:5px;" $ text "Clean cache"
        Nothing -> pure never
      return (eReset', eClear', eSave')
    let eNewPass = catMaybes (tagPromptlyDyn dmNewPass eSave)
    performEvent_ (saveHashedTextToStorage passwordSotrageKey "" <$ eClear)
    performEvent_ (saveHashedTextToStorage passwordSotrageKey . getPassRaw <$>
      eNewPass)
    widgetHold_ blank $ leftmost
      [ elAttr "div" ("class" =: "app-columns w-row" <> "style" =:
        "display:flex;justify-content:center;") (text "Password saved!") <$ eNewPass
      , elAttr "div" ("class" =: "app-columns w-row" <> "style" =:
        "display:flex;justify-content:center;") (text "Password cleared!") <$ eClear
      , blank <$ eOpen ]
    return (leftmost [Just <$> eNewPass, Nothing <$ eClear], eReset)
  where
    mkErr _ Nothing = blank
    mkErr _ (Just (PasswordRaw "")) = blank
    mkErr c _ = bool (errDiv "Incorrect password") blank c
    checkPass hash (Just raw) = checkPassword (getPassHash hash) (getPassRaw raw)
    checkPass _ _ = return False
    cls = "button-switching inverted flex-center"
    mkSaveBtnCls Nothing _ (Just _) = cls
    mkSaveBtnCls (Just _) True (Just _) = cls
    mkSaveBtnCls _ _ _ = cls <> " button-disabled"
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
  (eOk, eCancel) <- dialogWindow True eOpen (leftmost [eOk,eCancel]) "width: 60%" "" $ do
    divClass "connect-title-div" $ divClass "app-text-semibold" $ do
        text "This action will reset password and clean cache (the list of known coins)!"
        br
        text "Are you sure?"
    elAttr "div" ("class" =: "app-columns w-row" <> "style" =: "display:flex;justify-content:center;") $ do
      btnOk <- btn "button-switching inverted flex-center"
        "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      btnCancel <- btn "button-switching flex-center"
        "width:30%;display:inline-block;margin-left:5px;" $ text "Cancel"
      return (btnOk, btnCancel)
  performEvent_ (saveHashedTextToStorage passwordSotrageKey "" <$ eOk)
  performEvent_ ((saveJSON Nothing "encoins" . decodeUtf8 . toStrict $ encode @Text "") <$ eOk)
  return eOk
