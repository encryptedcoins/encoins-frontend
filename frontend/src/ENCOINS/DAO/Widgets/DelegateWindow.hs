module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Reflex.Dom

import           Backend.Protocol.Types
import           ENCOINS.App.Widgets.Basic              (elementResultJS, containerApp)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Events (addFocusPostBuildDelayE, logEvent, newEventWithDelay)
import           Backend.Wallet (Wallet(..), toJS)
import           JS.DAO (daoDelegateTx)
import           Data.Text (Text)
import ENCOINS.Common.Widgets.Basic (btn, divClassId)
import ENCOINS.Common.Utils (toText)
import Backend.Status (Status(..), otherError)
import Data.Text (unpack)
import Network.URI (parseURI)

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m (Event t Text, Event t ())
delegateWindow eOpen dWallet = mdo
  (eOk, eEscape) <- dialogWindow True eOpen (leftmost [void eOk, eEscape]) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" "" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Enter url:"

      (dUrlInp, eEscape) <- inputWidget eOpen (checkStatus <$> dStatus)

      addFocusPostBuildDelayE dUrlInp eOpen

      let dUrl = value dUrlInp

      btnOk <- buttonWidget $
        mconcat [checkStatus <$> dStatus, checkUrl <$> dUrl]

      logEvent "btnOk" btnOk

      let eUrl = traceEvent "eUrl" $ tagPromptlyDyn dUrl btnOk

      performEvent_
        $ daoDelegateTx <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eUrl

      isDelegate <- elementResultJS "Delegate" id
      logEvent "Delegate" $ checkEmptyText <$> updated isDelegate

      -- widgetHold_ blank $ leftmost [blank <$ eUrl, blank <$ eOpen]
      eStatus <- delegateStatus
      dStatus <- holdDyn Ready eStatus
      containerApp ""
        $ divClassId "app-text-small" "welcome-read-docs"
        $ dynText
        $ toText <$> traceDynWith (\dSt -> "dStatus: " <> show dSt) dStatus

      logEvent "eStatus" eStatus

      return (eUrl, eEscape)
  return (eOk, eEscape)


delegateStatus :: MonadWidget t m => m (Event t Status)
delegateStatus = do
  eConstruct <- updated <$> elementResultJS "DelegateCreateNewTx" id
  eSign <- updated <$> elementResultJS "DelegateSignTx" id
  eSubmit <- updated <$> elementResultJS "DelegateSubmitTx" id
  eErr <- updated <$> elementResultJS "DelegateError" id
  eErrStatus <- otherError eErr
  eCustom <- newEventWithDelay 5
  eCustomErr <- newEventWithDelay 15
  pure $ leftmost [
        -- Ready        <$ eConfirmed,
          eErrStatus
        , Constructing <$ eConstruct
        , Signing      <$ eSign
        , Submitting   <$ eSubmit
        , Signing <$ eCustom
        , OtherError "custom Error" <$ eCustomErr
        ]

inputWidget :: MonadWidget t m
  => Event t ()
  -> Dynamic t (Maybe ())
  -> m (InputElement EventResult GhcjsDomSpace t, Event t ())
inputWidget eOpen dStatus = divClass "app-columns w-row" $ do
    let dAttrs = maybe mempty (const $ "disabled" =: "") <$> dStatus
    modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "url"
          )
      & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    return (inp, keydown Escape inp)

buttonWidget :: MonadWidget t m
  => Dynamic t (Maybe ())
  -> m (Event t ())
buttonWidget dStatus = btn
    (traceDynWith (\cl -> "btClass: " <> unpack cl) $ mkBtnAttrs dStatus)
    "width:30%;display:inline-block;margin-right:5px;"
    (text "Ok")
  where
    btnOnAttrs = "button-switching inverted flex-center"
    btnOffAttrs = "button-switching inverted flex-center button-disabled"
    mkBtnAttrs dSt = maybe btnOnAttrs (const btnOffAttrs) <$> dSt

-- For checkUrl and checkStatus
-- Just () stays for constraint, for 'disabled' attribute
-- Nothing for mempty
-- By default used mempty.
-- Just () applied to disabled for able to use Monoid on Maybe.
checkUrl :: Text -> Maybe ()
checkUrl "" = Just ()
checkUrl t = case parseURI $ unpack t of
  Nothing -> Just ()
  Just _ -> Nothing

checkStatus :: Status -> Maybe ()
checkStatus status =
  if status `elem` [Constructing, Signing, Submitting]
    then Just ()
    else Nothing