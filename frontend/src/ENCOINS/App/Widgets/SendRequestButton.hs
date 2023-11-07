{-# LANGUAGE RecursiveDo #-}


module ENCOINS.App.Widgets.SendRequestButton where

import           Control.Monad                (void, (<=<))
import           Data.Functor                 ((<&>))
import           Data.Text                    (Text)
import           Reflex.Dom
import           Servant.Reflex               (BaseUrl (..))

import           Backend.Protocol.TxValidity  (TxValidity (..), txValidity)
import           Backend.Protocol.Types
import           Backend.Servant.Requests     (getRelayUrl, getRelayUrlE,
                                               statusRequestWrapper, eventMaybe)
import           Backend.Status               (Status (..), relayError)
import           Backend.Wallet               (Wallet (..))
import           ENCOINS.App.Widgets.Basic    (relayStatusM, tellRelayStatus)
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
  -> m (Event t ())
sendRequestButton mode dStatus dWallet dCoinsToBurn dCoinsToMint e = mdo
  -- Getting the current MaxAda

  -- TODO: choose firing every time by 'e' fires
  -- or every time user enters the tab.
  ev <- newEvent
  logEvent "sendRequestButton: ev:" ev
  emUrl <- getRelayUrlE $ leftmost [ev, () <$ eRelayDown]
  logEvent "sendRequestButton: emUrl:" emUrl

  -- emUrlFallBack <- getRelayUrlE $ () <$ eRelayDown
  -- logEvent "sendRequestButton: emUrlFallBack:" emUrlFallBack
  -- tellRelayStatus emUrl
  dmUrl <- holdDyn Nothing emUrl
  -- logDyn "sendRequestButton: updated dmUrl:" dmUrl

  eFireStatus <- delay 1 $ leftmost [e, () <$ eRelayDown]
  -- logEvent "sendRequestButton: eFallback:" eFallback

  emStatus <- switchHold never <=< dyn $ dmUrl <&> \case
    Nothing -> pure never
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
  return $ () <$ ffilter (== TxValid) (current dTxValidity `tag` eSend)
