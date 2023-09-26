{-# LANGUAGE LambdaCase #-}
module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic              (elementResultJS, containerApp)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Events (addFocusPostBuildDelayE, logEvent)
import           Backend.Wallet (Wallet(..), toJS, lucidConfig)
import qualified JS.DAO as JS
import           Data.Text (Text)
import           ENCOINS.Common.Widgets.Basic (btn, divClassId)
import           ENCOINS.Common.Utils (toText)
import           Backend.Status (Status(..), otherError)
import           Data.Monoid (Any(..))
import           Data.Bool (bool)
import qualified Data.Text as T

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m (Dynamic t Status)
delegateWindow eOpen dWallet = mdo
  (eUrlOk, eEscape, dStatus) <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk, eEscape])
    "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px"
    "" $ mdo
          divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Enter url:"

          (dInputText, eEscape) <- inputWidget eOpen (isDisableStatus <$> dStatus)
          let eInputText = updated dInputText

          let eNonEmptyUrl = ffilter (not . T.null) eInputText
          let eEmptyUrl = ffilter T.null eInputText

          -- Check url when it is ONLY not empty
          performEvent_ (JS.checkUrl <$> eNonEmptyUrl)

          eValidUrl <- updated <$> elementResultJS "ValidUrl" id
          eInvalidUrl <- updated <$> elementResultJS "InvalidUrl" id

          let eIsInvalidUrl = leftmost
                [
                  True  <$ eEmptyUrl
                , True  <$ eInvalidUrl
                , False <$ eValidUrl
                ]

          dIsInvalidUrl <- holdDyn True eIsInvalidUrl

          let dIsDisableStatus = isDisableStatus <$> dStatus

          let eUrl = tagPromptlyDyn dInputText btnOk
          performEvent_
            $ JS.daoDelegateTx lucidConfig <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eUrl

          eStatus <- delegateStatus
          logEvent "Status" eStatus
          dStatus <- holdDyn Ready eStatus

          -- The button disable with invalid url and performant statuses.
          btnOk <- divClass "app-top-menu-div" $ mdo
            btnOk <- divClass "menu-item-button-right" $ do
              buttonWidget $ fmap getAny $ mconcat $ fmap (fmap Any)
                [ dIsDisableStatus, dIsInvalidUrl]
            divClass "menu-item-button-right" $ do
              containerApp ""
                $ divClassId "app-text-small" "delegation-status"
                $ dynText
                $ toText <$> dStatus
            pure btnOk

          return (eUrl, eEscape, dStatus)
  return dStatus

delegateStatus :: MonadWidget t m
  => m (Event t Status)
delegateStatus = do
  eConstruct <- updated <$> elementResultJS "DelegateCreateNewTx" id
  eSign <- updated <$> elementResultJS "DelegateSignTx" id
  eSubmit <- updated <$> elementResultJS "DelegateSubmitTx" id
  eSubmitted <- updated <$> elementResultJS "DelegateSubmitedTx" id
  eReady <- updated <$> elementResultJS "DelegateReadyTx" id
  eErr <- updated <$> elementResultJS "DelegateError" id
  eErrStatus <- otherError eErr
  pure $ leftmost [
          eErrStatus
        , Ready                            <$ eReady
        , Constructing                     <$ eConstruct
        , Signing                          <$ eSign
        , Submitting                       <$ eSubmit
        , Submitted                        <$ eSubmitted
        ]

inputWidget :: MonadWidget t m
  => Event t ()
  -> Dynamic t Bool
  -> m (Dynamic t Text, Event t ())
inputWidget eOpen dDisableStatus = divClass "w-row" $ do
    let dAttrs = bool mempty ("disabled" =: "") <$> dDisableStatus
    modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "url"
          )
      & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
      & inputElementConfig_setValue .~ ("" <$ eOpen)

    addFocusPostBuildDelayE inp eOpen

    return (value inp, keydown Escape inp)

buttonWidget :: MonadWidget t m
  => Dynamic t Bool
  -> m (Event t ())
buttonWidget dStatus = btn
    (mkBtnAttrs dStatus)
    "width:30%;display:inline-block;margin-right:5px;"
    (text "Ok")
  where
    btnOnAttrs = "button-switching inverted flex-center"
    btnOffAttrs = "button-switching inverted flex-center button-disabled"
    mkBtnAttrs dSt = bool btnOnAttrs btnOffAttrs <$> dSt

-- Check if status is the performant one.
-- Performant status fires when background operations are processing.
isDisableStatus :: Status -> Bool
isDisableStatus status = status `elem` [Constructing, Signing, Submitting, Submitted]