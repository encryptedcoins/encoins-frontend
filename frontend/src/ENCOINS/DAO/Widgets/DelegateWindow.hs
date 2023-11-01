{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RecursiveDo    #-}

module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                   (forM, void, (<=<))
import           Data.Functor                    ((<&>))
import           Data.List                       (sortOn)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)
import           Data.Ord                        (Down (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Numeric.Natural                 (Natural)
import           Reflex.Dom

import           Backend.Status                  (UrlStatus (..), isNotValidUrl)
import           Backend.Wallet                  (Wallet (..), lucidConfigDao,
                                                  toJS)
import           ENCOINS.App.Widgets.Basic       (containerApp, elementResultJS)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (toText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn, btnWithBlock, divClassId)
import qualified JS.DAO                          as JS

delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> m ()
delegateWindow eOpen dWallet = mdo
  eUrlOk <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk])
    "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px);"
    "Delegate Encoins" $ mdo

          eUrlTable <- relayAmountWidget eOpen

          divClass "dao-DelegateWindow_EnterUrl" $ text "Enter relay url:"

          dInputText <- inputWidget eOpen
          let eInputText = updated dInputText

          let eNonEmptyUrl = ffilter (not . T.null) eInputText
          let eEmptyUrl = ffilter T.null eInputText

          -- Check url when it is ONLY not empty
          performEvent_ (JS.checkUrl <$> eNonEmptyUrl)
          eValidUrl <- updated <$> elementResultJS "ValidUrl" id
          eInvalidUrl <- updated <$> elementResultJS "InvalidUrl" id

          -- ePing <- pingRequestWrapper (BasePath . normalizePingUrl <$> dInputText) $ () <$ eValidUrl

          let eUrlStatus = leftmost
                [
                  UrlEmpty   <$ eEmptyUrl
                , UrlInvalid <$ eInvalidUrl
                , UrlValid   <$ eValidUrl
                -- , maybe UrlPingFail (const UrlPingSuccess) <$> ePing
                ]

          dIsInvalidUrl <- holdDyn UrlEmpty eUrlStatus
          -- The button disable with invalid url and performant status.
          btnOk <- buttonWidget dIsInvalidUrl

          let eUrlButton = tagPromptlyDyn dInputText btnOk
          logEvent "eUrlButton" eUrlButton

          let eUrl = leftmost [eUrlTable, eUrlButton]
          logEvent "eUrl" eUrl
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
        (isNotValidUrl <$> dUrlStatus)
        (text "Delegate")
    divClass "menu-item-button-right" $ do
      containerApp ""
        $ divClassId "app-text-small" ""
        $ dynText
        $ toText <$> dUrlStatus
    pure eButton

relayAmountWidget :: MonadWidget t m
  => Event t ()
  -> m (Event t Text)
relayAmountWidget eOpen = do
  dRelays <- holdDyn [] =<< fetchRelayTable eOpen
  article $ tableWrapper $
    table $ do
      el "thead" $ tr $
        mapM_ (\h -> th $ text h) ["Relay", "Amount", ""]
      el "tbody" $ do
        switchHold never <=< dyn $ dRelays <&> \relays -> do
          evs <- forM relays $ \(relay, amount) -> tr $ do
            tdRelay $ text relay
            tdAmount $ text $ toText amount
            eClick <- tdButton $ btn "button-switching inverted" "" $ text "Delegate"
            pure $ relay <$ eClick
          pure $ leftmost evs
  where
    tableWrapper = elAttr "div" ("class" =: "dao-DelegateWindow_TableWrapper")
    table = elAttr "table" ("class" =: "dao-DelegateWindow_Table")
    article = elAttr "article" ("class" =: "dao-DelegateWindow_RelayAmount")
    tr = elAttr "tr" ("class" =: "dao-DelegateWindow_TableRow")
    th = elAttr "th" ("class" =: "dao-DelegateWindow_TableHeader")
    tdRelay = elAttr "td" ("class" =: "dao-DelegateWindow_TableRelay")
    tdAmount = elAttr "td" ("class" =: "dao-DelegateWindow_TableAmount")
    tdButton = elAttr "td" ("class" =: "dao-DelegateWindow_TableButton")

sortRelayAmounts :: Maybe (Map Text String) -> [(Text, Integer)]
sortRelayAmounts =
    sortOn (Down . snd)
  . Map.toList
  . Map.map (floor @Double . (\x -> fromIntegral x / 1000000) . read @Natural )
  . fromJust

fetchRelayTable :: MonadWidget t m
  => Event t ()
  -> m (Event t [(Text, Integer)])
fetchRelayTable eOpen = do
  let eUrl = "https://encoins.io/delegations.json" <$ eOpen
  fmap sortRelayAmounts <$> getAndDecode eUrl