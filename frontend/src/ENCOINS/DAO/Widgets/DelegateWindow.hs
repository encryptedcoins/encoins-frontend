{-# LANGUAGE LambdaCase #-}
module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void)
import           Data.Text                              (Text)
import qualified Data.Text as T
import           Reflex.Dom

import           ENCOINS.App.Widgets.Basic              (elementResultJS, containerApp)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Widgets.Basic           (btnWithBlock, divClassId)
import           ENCOINS.Common.Events                  (addFocusPostBuildDelayE)
import           Backend.Wallet                         (Wallet(..), toJS, lucidConfig)
import           Backend.Status                         (UrlStatus(..), isNotValidUrl)
import qualified JS.DAO as JS
import           ENCOINS.Common.Utils (toText)

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m ()
delegateWindow eOpen dWallet = mdo
  eUrlOk <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk])
    "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px"
    "" $ mdo
          divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Enter url:"

          dInputText <- inputWidget eOpen
          let eInputText = updated dInputText

          let eNonEmptyUrl = ffilter (not . T.null) eInputText
          let eEmptyUrl = ffilter T.null eInputText

          -- Check url when it is ONLY not empty
          performEvent_ (JS.checkUrl <$> eNonEmptyUrl)

          eValidUrl <- updated <$> elementResultJS "ValidUrl" id
          eInvalidUrl <- updated <$> elementResultJS "InvalidUrl" id

          let eUrlStatus = leftmost
                [
                  UrlEmpty   <$ eEmptyUrl
                , UrlInvalid <$ eInvalidUrl
                , UrlValid   <$ eValidUrl
                ]
          dIsInvalidUrl <- holdDyn UrlEmpty eUrlStatus
          -- The button disable with invalid url and performant status.
          btnOk <- buttonWidget dIsInvalidUrl

          let eUrl = tagPromptlyDyn dInputText btnOk

          performEvent_ $
            JS.daoDelegateTx lucidConfig
            <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eUrl

          return eUrl
  pure ()

inputWidget :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Text)
inputWidget eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "relay url"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    addFocusPostBuildDelayE inp eOpen
    return $ value inp

buttonWidget :: MonadWidget t m
  => Dynamic t UrlStatus
  -> m (Event t ())
buttonWidget dUrlStatus =
  divClass "app-top-menu-div menu-item-button-right" $ do
    b <- btnWithBlock
        "button-switching inverted flex-center"
        "width:30%;display:inline-block;margin-right:5px;"
        (isNotValidUrl <$> dUrlStatus)
        (text "Ok")
    divClass "menu-item-button-right" $ do
      containerApp ""
        $ divClassId "app-text-small" "url-status"
        $ dynText
        $ toText <$> dUrlStatus
    pure b
