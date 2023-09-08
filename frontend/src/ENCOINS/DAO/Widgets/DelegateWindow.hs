module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Reflex.Dom

import           Backend.Protocol.Types
import           ENCOINS.App.Widgets.Basic              (elementResultJS, containerApp)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Events (addFocusPostBuildDelayE, logEvent)
import           Backend.Wallet (Wallet(..), toJS)
import           JS.DAO (daoDelegateTx)
import           Data.Text (Text)
import ENCOINS.Common.Widgets.Basic (btn, divClassId)
import ENCOINS.Common.Utils (toText)
import Backend.Status (Status(..), otherError)


delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m (Event t Text, Event t ())
delegateWindow eOpen dWallet = mdo
  (eOk, eEscape) <- dialogWindow True eOpen (leftmost [void eOk, eEscape]) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" "" $ mdo
      divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Enter url:"

      (dUrlInp, eEscape) <- divClass "app-columns w-row" $ do
        inp <- inputElement $ def
          & initialAttributes .~
              ( "class" =: "w-input"
              <> "style" =: "display: inline-block;"
              <> "placeholder" =: "url"
              )
          & inputElementConfig_setValue .~ ("" <$ eOpen)
        return (inp, keydown Escape inp)

      addFocusPostBuildDelayE dUrlInp eOpen

      btnOk <- btn btnAttrs
        "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"

      let eUrl = traceEvent "eUrl" $ tagPromptlyDyn (value dUrlInp) btnOk

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
        $ fmap toText dStatus

      return (eUrl, eEscape)
  return (eOk, eEscape)
  where
    btnAttrs = "button-switching inverted flex-center"

delegateStatus :: MonadWidget t m => m (Event t Status)
delegateStatus = do
  eConstruct <- updated <$> elementResultJS "DelegateCreateNewTx" id
  eSign <- updated <$> elementResultJS "DelegateSignTx" id
  eSubmit <- updated <$> elementResultJS "DelegateSubmitTx" id
  eErr <- updated <$> elementResultJS "DelegateError" id
  eErrStatus <- otherError eErr
  pure $ leftmost [
        -- Ready        <$ eConfirmed,
        eErrStatus,
        Constructing <$ eConstruct,
        Signing      <$ eSign,
        Submitting   <$ eSubmit
        ]