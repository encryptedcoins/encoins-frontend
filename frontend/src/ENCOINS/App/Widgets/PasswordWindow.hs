{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.PasswordWindow where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isLower, isUpper, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import Witherable (catMaybes)

import Backend.Protocol.StrongTypes (PasswordHash (getPassHash), toPasswordHash)
import Backend.Protocol.Types (PasswordRaw (..))
import Common.Events
import Common.Events (setFocusDelayOnEvent)
import Common.Reflex.Extra (switchHoldDyn)
import Common.Utility (hashKeccak512, isHashOfRaw)
import ENCOINS.Common.Cache (encoinsV3, passwordStorageKey, saveAppData_)
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Basic (br, btn, divClassDyn)
import JS.App (loadCacheValue, saveHashedTextToStorage)

validatePassword :: Text -> Either Text PasswordRaw
validatePassword txt
    | not $ T.all validPasswordChar txt =
        Left
            "Password must consist of \
            \uppercase and lowercase letters, numbers, and special characters"
    | T.length txt < 10 = Left "Password must be at least 10 characters long"
    | not (T.any isUpper txt) =
        Left "Password must contain at least one upper-case letter"
    | not (T.any isLower txt) =
        Left "Password must contain at least one lower-case letter"
    | not (T.any isSpecial txt) =
        Left "Password must contain at least one special character"
    | otherwise = Right (PasswordRaw txt)

-- Uppercase and lowercase letters, numbers, and special characters from the
-- ASCII character set. (i.e. everything from the ASCII set except the control characters)
validPasswordChar :: Char -> Bool
validPasswordChar c = ord c >= 32 && ord c <= 126

isSpecial :: Char -> Bool
isSpecial c =
    validPasswordChar c
        && not (isAsciiUpper c || isAsciiLower c || isDigit c)

enterPasswordWindow ::
    (MonadWidget t m) =>
    PasswordHash
    -> Event t ()
    -> m (Event t PasswordRaw, Event t ())
enterPasswordWindow passHash eResetOk = mdo
    dWindowIsOpen <- holdDyn True (False <$ leftmost [void eClose, eResetOk])
    ret@(eClose, _) <- do
        divClassDyn (mkClass <$> dWindowIsOpen) $ mdo
            (eClean, eOk, dPass) <- viewEnterPasswordEntries eError
            let dPassOk = checkPass passHash <$> dPass
            let eError =
                    leftmost
                        [ maybe (viewPasswordError "Incorrect password") (const blank)
                            <$> tagPromptlyDyn dPassOk eOk
                        , blank <$ updated dPassOk
                        ]
            pure (catMaybes $ tagPromptlyDyn dPassOk eOk, eClean)
    pure ret
    where
        mkClass b = bool "app-EnterPasswordWindow-none" "app-EnterPasswordWindow" b
        checkPass hash mRaw = do
            raw <- mRaw
            if isHashOfRaw (getPassHash hash) (getPassRaw raw)
                then Just raw
                else Nothing

viewEnterPasswordEntries ::
    (MonadWidget t m) =>
    Event t (m ()) -- password error
    -> m (Event t (), Event t (), Dynamic t (Maybe PasswordRaw))
viewEnterPasswordEntries eError = divClass "app-DialogWindow_EnterPassword" $ mdo
    divClass "w-row" $
        divClass "connect-title-div" $
            divClass "app-text-semibold" $
                text "Password for the cache of Encoins app"
    dPass' <- divClass "w-row" $
        divClass "w-col w-col-12" $ do
            ePb <- getPostBuild
            passwordInput "Enter password:" False True (pure Nothing) eError ePb
    (eClean, eSave) <- divClass "w-row app-EnterPassword_ButtonContainer" $ do
        eSave' <-
            btn
                "button-switching inverted flex-center"
                ""
                $ text "Ok"
        eClean' <-
            btn
                "button-switching flex-center"
                ""
                $ text "Clean cache"
        pure (eClean', eSave')
    pure (eClean, eSave, dPass')

passwordSettingsWindow ::
    (MonadWidget t m) =>
    Event t ()
    -> m (Event t (Maybe PasswordRaw), Event t ())
passwordSettingsWindow eOpen = mdo
    ePassHash <- performEvent (loadCacheValue passwordStorageKey <$ eOpen)
    let emPassHash = toPasswordHash <$> ePassHash
    dmPassHash <- holdDyn Nothing emPassHash
    dialogWindow
        True
        eOpen
        never
        "app-PasswordSettingsWindow"
        "Protect cache of Encoins app"
        $ do
            dPassOk <- passwordChecker dmPassHash eOpen

            dmNewPass <- passwordEnterRepeat eOpen

            (eReset, eClear, eSave) <- passwordButtons dmPassHash dPassOk dmNewPass

            let eNewPass = catMaybes (tagPromptlyDyn dmNewPass eSave)

            passwordNotification eNewPass eReset eOpen

            let emChangedPassword = leftmost [Just <$> eNewPass, Nothing <$ eReset]

            performEvent_ $
                saveHashedTextToStorage passwordStorageKey . hashKeccak512 . maybe "" getPassRaw
                    <$> emChangedPassword

            pure (emChangedPassword, eClear)

passwordButtons ::
    (MonadWidget t m) =>
    Dynamic t (Maybe PasswordHash)
    -> Dynamic t Bool
    -> Dynamic t (Maybe PasswordRaw)
    -> m (Event t (), Event t (), Event t ())
passwordButtons dmPassHash dPassOk dmNewPass = do
    let cls = "button-switching inverted flex-center"
        mkSaveBtnCls Nothing _ (Just _) = cls
        mkSaveBtnCls (Just _) True (Just _) = cls
        mkSaveBtnCls _ _ _ = cls <> " button-disabled"
        mkClearBtnCls = (cls <>) . bool " button-disabled" ""
    divClass
        "w-row app-PasswordSetting_ButtonContainer"
        $ do
            eSave' <- do
                let dSaveClass = mkSaveBtnCls <$> dmPassHash <*> dPassOk <*> dmNewPass
                btn dSaveClass "" $ text "Save"
            eReset' <- switchHoldDyn dmPassHash $ \case
                Just _ ->
                    btn (mkClearBtnCls <$> dPassOk) "white-space: nowrap;" $ text "Reset password"
                Nothing -> pure never
            eClear' <- switchHoldDyn dmPassHash $ \case
                Just _ ->
                    btn "button-switching flex-center" "white-space: nowrap;" $ text "Clean cache"
                Nothing -> pure never
            pure (eReset', eClear', eSave')

passwordNotification ::
    (MonadWidget t m) =>
    Event t PasswordRaw
    -> Event t ()
    -> Event t ()
    -> m ()
passwordNotification eNewPass eReset eOpen =
    widgetHold_ blank $
        leftmost
            [ divClass "app-PasswordWindow_Notification" (text "Password saved!") <$ eNewPass
            , divClass "app-PasswordWindow_Notification" (text "Password cleared!") <$ eReset
            , blank <$ eOpen
            ]

passwordChecker ::
    (MonadWidget t m) =>
    Dynamic t (Maybe PasswordHash)
    -> Event t ()
    -> m (Dynamic t Bool)
passwordChecker dmPassHash eOpen = do
    let mkErr _ Nothing = blank
        mkErr _ (Just (PasswordRaw "")) = blank
        mkErr c _ = bool (viewPasswordError "Incorrect password") blank c
        checkPass hash (Just raw) = isHashOfRaw (getPassHash hash) (getPassRaw raw)
        checkPass _ Nothing = False
    ePassOk <- switchHoldDyn dmPassHash $ \case
        Just passHash -> divClass "w-row" $ divClass "w-col w-col-12" $ mdo
            dmCurPass <-
                passwordInput "Current password:" False True (pure Nothing) eError eOpen
            let dCheckedPass = checkPass passHash <$> dmCurPass
            let eError = updated $ mkErr <$> dCheckedPass <*> dmCurPass
            return (ffilter id $ updated dCheckedPass)
        Nothing -> pure never
    holdDyn False ePassOk

passwordEnterRepeat ::
    (MonadWidget t m) =>
    Event t ()
    -> m (Dynamic t (Maybe PasswordRaw))
passwordEnterRepeat eOpen =
    divClass "app-PasswordProtect_Window" $ do
        dmPass1 <- divClass "w-col" $ do
            passwordInput "Enter password:" False True (pure Nothing) never eOpen
        dmPass2 <- divClass "w-col" $ do
            passwordInput "Repeat password:" True False dmPass1 never eOpen
        return dmPass2

passwordInput ::
    (MonadWidget t m) =>
    Text
    -> Bool
    -> Bool
    -> Dynamic t (Maybe PasswordRaw)
    -> Event t (m ()) -- Incorrect password error
    -> Event t ()
    -> m (Dynamic t (Maybe PasswordRaw))
passwordInput txt rep isFocus dmPass eError eOpen = mdo
    dShowPass <- toggle False (domEvent Click eye)
    divClass "app-PasswordError_Container" $ do
        appTextLeft txt
        dyn_ $ mkError <$> value inp <*> deVal <*> dmPass -- view invalid password input error
        widgetHold_ blank eError -- view incorrect password error
    inp <- inputElement $ conf $ bool "password" "text" <$> updated dShowPass
    if isFocus then setFocusDelayOnEvent inp eOpen else blank
    (eye, _) <- elDynAttr' "i" (mkEyeAttr <$> dShowPass) blank
    let deVal = validatePassword <$> value inp
    return (zipDynWith mkRes deVal dmPass)
    where
        mkRes (Right p1) (Just p2) =
            if rep
                then
                    if p1 == p2
                        then Just p1
                        else Nothing
                else Just p1
        mkRes ep1 _ = either (const Nothing) Just ep1
        mkError "" _ _ = blank
        mkError _ (Right p1) (Just p2) =
            if rep
                then
                    if p1 == p2
                        then blank
                        else viewPasswordError "Password doesn't match"
                else blank
        mkError _ (Right _) Nothing =
            if rep
                then viewPasswordError "Password doesn't match"
                else blank
        mkError _ (Left err) _ =
            if rep
                then viewPasswordError "Password doesn't match"
                else viewPasswordError err
        mkEyeAttr showPass = "class" =: ("app-Eye_Input far " <> bool "fa-eye" "fa-eye-slash" showPass)
        appTextLeft = divClass "app-Password_InputTitle" . text
        conf eType =
            def
                & initialAttributes
                .~ ( "class" =: "app-Input_Password"
                        <> "type" =: "password"
                   )
                & inputElementConfig_initialValue
                .~ ""
                & inputElementConfig_elementConfig
                . elementConfig_modifyAttributes
                .~ (("type" =:) . Just <$> eType)
                & inputElementConfig_setValue
                .~ ("" <$ eOpen)

cleanCacheDialog :: (MonadWidget t m) => Event t () -> m (Event t ())
cleanCacheDialog eOpen = mdo
    (eOk, eCancel) <- dialogWindow
        True
        eOpen
        (leftmost [eOk, eCancel])
        "app-CleanCacheWindow"
        "Clean cache"
        $ do
            divClass "app-CleanCache_Description" $ do
                text "This action will reset password and clean cache (remove known coins)!"
                br
                text "Are you sure?"
            divClass "w-row app-CleanCache_ButtonContainer" $ do
                btnOk <- btn "button-switching inverted flex-center" "" $ text "Clean"
                btnCancel <- btn "button-switching flex-center" "" $ text "Cancel"
                return (btnOk, btnCancel)
    performEvent_
        (saveHashedTextToStorage passwordStorageKey (hashKeccak512 "") <$ eOk)
    saveAppData_ Nothing encoinsV3 $ ("" :: Text) <$ eOk
    return eOk

viewPasswordError :: (MonadWidget t m) => Text -> m ()
viewPasswordError = divClass "app-PasswordError_Message" . text
