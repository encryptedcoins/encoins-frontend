{-# LANGUAGE LambdaCase #-}
module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Data.Text                              (Text)
import qualified Data.Text as T
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic              (elementResultJS)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Widgets.Basic           (btnWithBlock)
import           ENCOINS.Common.Events                  (addFocusPostBuildDelayE)
import           Backend.Wallet                         (Wallet(..), toJS, lucidConfig)
import qualified JS.DAO as JS

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m ()
delegateWindow eOpen dWallet = mdo
  (eUrlOk, eEscape) <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk, eEscape])
    "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px"
    "" $ mdo
          divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Enter url:"

          (dInputText, eEscape) <- inputWidget eOpen
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

          let eUrl = tagPromptlyDyn dInputText btnOk
          performEvent_
            $ JS.daoDelegateTx lucidConfig <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eUrl

          -- The button disable with invalid url and performant statuses.
          btnOk <- divClass "app-top-menu-div menu-item-button-right" $ mdo
            btnWithBlock
                "button-switching inverted flex-center"
                "width:30%;display:inline-block;margin-right:5px;"
                dIsInvalidUrl
                "Ok"
            -- divClass "menu-item-button-right" $ do
            --   containerApp ""
            --     $ divClassId "app-text-small" "delegation-status"
            --     $ dynText
            --     $ toText <$> dStatus
            -- pure btnOk

          return (eUrl, eEscape)
  pure ()

inputWidget :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Text, Event t ())
inputWidget eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "url"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)

    addFocusPostBuildDelayE inp eOpen

    return (value inp, keydown Escape inp)
