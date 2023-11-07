{-# LANGUAGE RecursiveDo #-}


module ENCOINS.App.Widgets.SendRequestButton where

import           Control.Monad                (void, (<=<))
import           Data.Functor                 ((<&>))
import           Data.Text                    (Text)
import           Reflex.Dom
import           Servant.Reflex               (BaseUrl (..))

import           Backend.Protocol.TxValidity  (TxValidity (..), txValidity)
import           Backend.Protocol.Types
import           Backend.Servant.Requests     (eventMaybe, getRelayUrl,
                                               getRelayUrlE,
                                               statusRequestWrapper)
import           Backend.Status               (Status (..), relayError)
import           Backend.Utility              (toEither)
import           Backend.Wallet               (Wallet (..))
import           ENCOINS.App.Widgets.Basic    (tellTxStatus)
import           ENCOINS.Bulletproofs         (Secrets)
import           ENCOINS.Common.Events        (logDyn, logEvent, newEvent)
import           ENCOINS.Common.Widgets.Basic (btn, divClassId)

sendRequestButton :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => EncoinsMode
  -> Dynamic t Status
  -> Dynamic t Wallet
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> m (Event t Status, Event t ())
sendRequestButton mode dStatus dWallet dCoinsToBurn dCoinsToMint e = mdo

  -- TODO: choose url every time by 'e' fires
  -- or every time user enters the tab.
  eInit <- newEvent
  logEvent "sendRequestButton: eInit:" eInit
  emUrl <- getRelayUrlE $ leftmost [eInit, () <$ eRelayDown]
  logEvent "sendRequestButton: emUrl:" emUrl

  let eAllRelayDown = filterLeft $ toEither () <$> emUrl
  tellTxStatus "" $ NoRelay <$ eAllRelayDown

  dmUrl <- holdDyn Nothing emUrl

  eFireStatus <- delay 1 $ leftmost [e, () <$ eRelayDown]

  -- Getting current MaxAda
  emStatus <- switchHold never <=< dyn $ dmUrl <&> \case
    Nothing  -> pure never
    Just url -> statusRequestWrapper url (pure MaxAdaWithdraw) eFireStatus
  let (eMaxAda, eRelayDown) = eventMaybe (BackendError relayError) emStatus

  let getMaxAda (MaxAdaWithdrawResult n) = Just n
      getMaxAda _                        = Nothing
  dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)

  -- SEND REQUEST button
  let dTxValidity = txValidity mode
        <$> dmUrl
        <*> dMaxAda
        <*> dStatus
        <*> dWallet
        <*> dCoinsToBurn
        <*> dCoinsToMint
      f v = case v of
          TxValid -> "button-switching flex-center"
          _       -> "button-not-selected button-disabled flex-center"
      g v = case v of
          TxValid       -> blank
          TxInvalid err -> elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
              "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $
              divClass "app-text-normal" $ text err
      h v = case v of
        TxValid -> ""
        _       -> "border-bottom-left-radius: 0px; border-bottom-right-radius: 0px"
  eSend <- divClassId "" "welcome-send-req" $
    btn (fmap f dTxValidity) (fmap h dTxValidity) $ text "SEND REQUEST"
  dyn_ $ fmap g dTxValidity
  let eValidTx = () <$ ffilter (== TxValid) (current dTxValidity `tag` eSend)
  pure (NoRelay <$ eAllRelayDown, eValidTx)
