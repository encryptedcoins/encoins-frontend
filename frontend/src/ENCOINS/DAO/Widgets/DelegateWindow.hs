{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex.Dom

import           Backend.Status                  (UrlStatus (..), isNotValidUrl)
import           Backend.Wallet                  (LucidConfig (..), Wallet (..),
                                                  lucidConfigDao, toJS)
import           ENCOINS.App.Widgets.Basic       (containerApp)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (checkUrl, toText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btnWithBlock, divClassId)
import           ENCOINS.DAO.Widgets.DelegateWindow.RelayNames (relayNames)
import           ENCOINS.DAO.Widgets.RelayTable  (relayAmountWidget)
import qualified JS.DAO                          as JS

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> Dynamic t [(Text, Integer)]
  -> m ()
delegateWindow eOpen dWallet dRelays = mdo
  eUrlOk <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk])
    "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px);"
    "Delegate Encoins" $ mdo

          let dNames = map (\(ip, n) -> (fromMaybe ip (ip `Map.lookup` relayNames), n)) <$> dRelays
          eUrlTable <- relayAmountWidget dNames

          divClass "dao-DelegateWindow_EnterUrl" $ text "Enter relay url:"

          dInputText <- inputWidget eOpen
          let eInputText = updated dInputText

          let eNonEmptyUrl = ffilter (not . T.null) eInputText
          let eEmptyUrl = ffilter T.null eInputText

          -- ePing <- pingRequestWrapper (BasePath . normalizePingUrl <$> dInputText) $ () <$ eValidUrl

          let eUrlStatus = leftmost
                [
                  UrlEmpty   <$ eEmptyUrl
                -- Check url when it is ONLY not empty
                , bool UrlInvalid UrlValid . checkUrl <$> eNonEmptyUrl
                -- , maybe UrlPingFail (const UrlPingSuccess) <$> ePing
                ]

          dIsInvalidUrl <- holdDyn UrlEmpty eUrlStatus
          -- The button disable with invalid url and performant status.
          btnOk <- buttonWidget dIsInvalidUrl

          let eUrlButton = tagPromptlyDyn dInputText btnOk
          logEvent "eUrlButton" eUrlButton

          let eUrl = leftmost [eUrlTable, eUrlButton]
          logEvent "eUrl" eUrl
          let LucidConfig apiKey networkId policyId assetName = lucidConfigDao
          performEvent_ $
            JS.daoDelegateTx apiKey networkId policyId assetName
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
          <> "placeholder" =: "url"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

buttonWidget :: MonadWidget t m
  => Dynamic t UrlStatus
  -> m (Event t ())
buttonWidget dUrlStatus =
  divClass "dao-DelegateWindow_ButtonStatusContainer" $ do
    eButton <- btnWithBlock
        "button-switching inverted flex-center"
        ""
        (isNotValidUrl <$> (UrlValid <$ dUrlStatus)) -- TODO: roll back it after debug
        (text "Delegate")
    divClass "menu-item-button-right" $ do
      containerApp ""
        $ divClassId "app-text-small" ""
        $ dynText
        $ toText <$> dUrlStatus
    pure eButton
