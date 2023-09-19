module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Reflex.Dom

import           Backend.Protocol.Types
import           ENCOINS.App.Widgets.Basic              (elementResultJS)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Events (addFocusPostBuildDelayE, logEvent)
import           Backend.Wallet (Wallet(..), toJS)
import           JS.DAO (daoDelegateTx)
import           Data.Text (Text)
import ENCOINS.Common.Widgets.Basic (btn)


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

      widgetHold_ blank $ leftmost [blank <$ eUrl, blank <$ eOpen]
      return (eUrl, eEscape)
  return (eOk, eEscape)
  where
    btnAttrs = "button-switching inverted flex-center"