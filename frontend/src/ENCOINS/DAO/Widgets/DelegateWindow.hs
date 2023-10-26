{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                          (void, forM_)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Numeric.Natural  (Natural)
import           Data.Text                              (Text)
import qualified Data.Text as T
import           Reflex.Dom
import           Servant.Reflex                         (BaseUrl (..))
import           Data.Aeson           (decode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Maybe             (fromJust)


import           ENCOINS.App.Widgets.Basic              (elementResultJS, containerApp)
import           ENCOINS.Common.Widgets.Advanced        (dialogWindow)
import           ENCOINS.Common.Widgets.Basic           (btnWithBlock, divClassId)
import           ENCOINS.Common.Events                  (setFocusDelayOnEvent)
import           Backend.Wallet                         (Wallet(..), toJS, lucidConfigDao)
import           Backend.Status                         (UrlStatus(..), isNotValidUrl)
import qualified JS.DAO as JS
import           ENCOINS.Common.Utils (toText)
import           Backend.Servant.Requests (pingRequestWrapper)
import           Config.Config (delegateRelayAmount)

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m ()
delegateWindow eOpen dWallet = mdo
  eUrlOk <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk])
    "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px)"
    "Delegate Encoins" $ mdo

          relayAmountWidget

          divClass "dao-DelegateWindow_EnterUrl" $ text "Enter url:"

          dInputText <- inputWidget eOpen
          let eInputText = updated dInputText

          let eNonEmptyUrl = ffilter (not . T.null) eInputText
          let eEmptyUrl = ffilter T.null eInputText

          -- Check url when it is ONLY not empty
          performEvent_ (JS.checkUrl <$> eNonEmptyUrl)

          eValidUrl <- updated <$> elementResultJS "ValidUrl" id
          eInvalidUrl <- updated <$> elementResultJS "InvalidUrl" id

          ePing <- pingRequestWrapper (BasePath . normalizePingUrl <$> dInputText) $ () <$ eValidUrl

          let eUrlStatus = leftmost
                [
                  UrlEmpty   <$ eEmptyUrl
                , UrlInvalid <$ eInvalidUrl
                , UrlValid   <$ eValidUrl
                , maybe UrlPingFail (const UrlPingSuccess) <$> ePing
                ]

          dIsInvalidUrl <- holdDyn UrlEmpty eUrlStatus
          -- The button disable with invalid url and performant status.
          btnOk <- buttonWidget dIsInvalidUrl

          let eUrl = tagPromptlyDyn dInputText btnOk

          performEvent_ $
            JS.daoDelegateTx lucidConfigDao
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
          <> "placeholder" =: "url of relay"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

buttonWidget :: MonadWidget t m
  => Dynamic t UrlStatus
  -> m (Event t ())
buttonWidget dUrlStatus =
  divClass "dao-DelegateWindow_ButtonStatusContainer" $ do
    b <- btnWithBlock
        "button-switching inverted flex-center"
        ""
        (isNotValidUrl <$> dUrlStatus)
        (text "Ok")
    divClass "menu-item-button-right" $ do
      containerApp ""
        $ divClassId "app-text-small" ""
        $ dynText
        $ toText <$> dUrlStatus
    pure b

normalizePingUrl :: Text -> Text
normalizePingUrl t = T.append (T.dropWhileEnd (== '/') t) "//"

relayAmountWidget :: MonadWidget t m => m ()
relayAmountWidget = do
  elAttr "div" ("class" =: "dao-DelegateWindow_TableWrapper") $
    el "table" $ do
      el "thead" $ tr $
        mapM_ (\h -> th $ text h) ["Relay", "Amount"]
      el "tbody" $
        forM_ (Map.toList relayAmounts) $ \(relay, amount) -> tr $ do
          td $ text relay
          td $ text $ toText amount

        -- listWithKey (constDyn relayAmounts) (\k r -> do
        --   elAttr "tr" "dao-DelegateWindow_TableRow" $
        --     mapM (\x -> elAttr "td" "dao-DelegateWindow_TableColumn" $ snd x k r) cols)

  where
    tr = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow")
    th = elAttr "th" ("class" =: "dao-DelegateWindow_TableHeader")
    td = elAttr "td" ("class" =: "dao-DelegateWindow_TableColumn")

relayAmounts :: Map Text Integer
relayAmounts =
  let parsedData :: Map Text String
        = fromJust
        $ decode
        $ fromStrict delegateRelayAmount
  in Map.map
      ( floor @Double
      . (\x -> fromIntegral x / 1000000)
      . read @Natural
      ) parsedData