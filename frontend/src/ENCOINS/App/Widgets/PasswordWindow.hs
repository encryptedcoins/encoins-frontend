{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.PasswordWindow where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isLower, isUpper, isNumber, ord)
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

import Common.Reflex.Dom.Extra
import I18n.I18n (App)
import qualified I18n.Common as I18n
import qualified I18n.App as I18n
import qualified I18n.I18n as I18n

validatePassword :: Text -> Either I18n.AppMessage PasswordRaw
validatePassword txt
    | not $ T.all validPasswordChar txt =
        Left I18n.PassInvalidNotAll
    | T.length txt < 10 = Left I18n.PassInvalidLess10
    | not (T.any isUpper txt) =
        Left I18n.PassInvalidNoUpper
    | not (T.any isLower txt) =
        Left I18n.PassInvalidNoLower
    | not (T.any isNumber txt) =
        Left I18n.PassInvalidNoNumber
    | not (T.any isSpecial txt) =
        Left I18n.PassInvalidNoSpecial
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
    forall t m.
    (App t m) =>
    PasswordHash
    -> Event t ()
    -> m (Event t PasswordRaw, Event t ())
enterPasswordWindow passHash eResetOk = mdo
    dWindowIsOpen <- holdDyn True (False <$ leftmost [void eClose, eResetOk])
    ret@(eClose, _) <- do
        divClassDyn (mkClass <$> dWindowIsOpen) $ mdo
            (eClean, eOk, dPass) <- viewEnterPasswordEntries eError
            dynTextLocale $ constDyn True
            let dPassOk = checkPass passHash <$> dPass
            let eError =
                    leftmost
                        [ maybe (viewPasswordError I18n.PassIncorrect) (const blank)
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
    (App t m) =>
    Event t (m ()) -- password error
    -> m (Event t (), Event t (), Dynamic t (Maybe PasswordRaw))
viewEnterPasswordEntries eError = divClass "app-DialogWindow_EnterPassword" $ mdo
    divClass "w-row" $
        divClass "connect-title-div" $
            divClass "app-text-semibold" $
                textLocale I18n.PassEntry
    dPass' <- divClass "w-row" $
        divClass "w-col w-col-12" $ do
            ePb <- getPostBuild
            passwordInput I18n.PassEnter False True (pure Nothing) eError ePb
    (eClean, eSave) <- divClass "w-row app-EnterPassword_ButtonContainer" $ do
        eSave' <-
            btn
                "button-switching inverted flex-center"
                ""
                $ textLocale I18n.Ok
        eClean' <-
            btn
                "button-switching flex-center"
                ""
                $ textLocale I18n.PassButtonClean
        pure (eClean', eSave')
    pure (eClean, eSave, dPass')

passwordSettingsWindow ::
    (App t m) =>
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
        (I18n.AppTerm I18n.PassWindowProtect)
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
    (App t m) =>
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
                btn dSaveClass "" $ textLocale I18n.Save
            eReset' <- switchHoldDyn dmPassHash $ \case
                Just _ ->
                    btn (mkClearBtnCls <$> dPassOk) "white-space: nowrap;" $ textLocale I18n.PassButtonReset
                Nothing -> pure never
            eClear' <- switchHoldDyn dmPassHash $ \case
                Just _ ->
                    btn "button-switching flex-center" "white-space: nowrap;" $ textLocale I18n.PassButtonClean
                Nothing -> pure never
            pure (eReset', eClear', eSave')

passwordNotification ::
    (App t m) =>
    Event t PasswordRaw
    -> Event t ()
    -> Event t ()
    -> m ()
passwordNotification eNewPass eReset eOpen =
    widgetHold_ blank $
        leftmost
            [ divClass "app-PasswordWindow_Notification" (textLocale I18n.PassSaved) <$ eNewPass
            , divClass "app-PasswordWindow_Notification" (textLocale I18n.PassCleared) <$ eReset
            , blank <$ eOpen
            ]

passwordChecker ::
    (App t m) =>
    Dynamic t (Maybe PasswordHash)
    -> Event t ()
    -> m (Dynamic t Bool)
passwordChecker dmPassHash eOpen = do
    let mkErr _ Nothing = blank
        mkErr _ (Just (PasswordRaw "")) = blank
        mkErr c _ = bool (viewPasswordError I18n.PassIncorrect) blank c
        checkPass hash (Just raw) = isHashOfRaw (getPassHash hash) (getPassRaw raw)
        checkPass _ Nothing = False
    ePassOk <- switchHoldDyn dmPassHash $ \case
        Just passHash -> divClass "w-row" $ divClass "w-col w-col-12" $ mdo
            dmCurPass <-
                passwordInput I18n.PassCurrent False True (pure Nothing) eError eOpen
            let dCheckedPass = checkPass passHash <$> dmCurPass
            let eError = updated $ mkErr <$> dCheckedPass <*> dmCurPass
            return (ffilter id $ updated dCheckedPass)
        Nothing -> pure never
    holdDyn False ePassOk

passwordEnterRepeat ::
    (App t m) =>
    Event t ()
    -> m (Dynamic t (Maybe PasswordRaw))
passwordEnterRepeat eOpen =
    divClass "app-PasswordProtect_Window" $ do
        dmPass1 <- divClass "w-col" $ do
            passwordInput I18n.PassEnter False True (pure Nothing) never eOpen
        dmPass2 <- divClass "w-col" $ do
            passwordInput I18n.PassRepeat True False dmPass1 never eOpen
        return dmPass2

passwordInput ::
    (App t m) =>
    I18n.AppMessage
    -> Bool
    -> Bool
    -> Dynamic t (Maybe PasswordRaw)
    -> Event t (m ()) -- Incorrect password error
    -> Event t ()
    -> m (Dynamic t (Maybe PasswordRaw))
passwordInput title rep isFocus dmPass eError eOpen = mdo
    dShowPass <- toggle False (domEvent Click eye)
    divClass "app-PasswordError_Container" $ do
        appTextLeft title
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
                        else viewPasswordError I18n.PassNotMatch
                else blank
        mkError _ (Right _) Nothing =
            if rep
                then viewPasswordError I18n.PassNotMatch
                else blank
        mkError _ (Left err) _ =
            if rep
                then viewPasswordError I18n.PassNotMatch
                else viewPasswordError err
        mkEyeAttr showPass = "class" =: ("app-Eye_Input far " <> bool "fa-eye" "fa-eye-slash" showPass)
        appTextLeft = divClass "app-Password_InputTitle" . textLocale
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

cleanCacheDialog :: (App t m) => Event t () -> m (Event t ())
cleanCacheDialog eOpen = mdo
    (eOk, eCancel) <- dialogWindow
        True
        eOpen
        (leftmost [eOk, eCancel])
        "app-CleanCacheWindow"
        (I18n.AppTerm I18n.CleanCacheWindowTitle)
        $ do
            divClass "app-CleanCache_Description" $ do
                textLocale I18n.CleanCacheText
                br
                textLocale I18n.AreYouSure
            divClass "w-row app-CleanCache_ButtonContainer" $ do
                btnOk <- btn "button-switching inverted flex-center" "" $ textLocale I18n.CleanCacheButtonClean
                btnCancel <- btn "button-switching flex-center" "" $ textLocale I18n.Cancel
                return (btnOk, btnCancel)
    performEvent_
        (saveHashedTextToStorage passwordStorageKey (hashKeccak512 "") <$ eOk)
    saveAppData_ Nothing encoinsV3 $ ("" :: Text) <$ eOk
    return eOk

viewPasswordError :: (App t m) => I18n.AppMessage -> m ()
viewPasswordError = divClass "app-PasswordError_Message" . textLocale
