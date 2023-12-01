{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.SendRequestButton where

import           Data.Text                    (Text)
import           Reflex.Dom

import           Backend.Protocol.TxValidity  (TxValidity (..),
                                               txValidityLedger,
                                               txValidityWallet)
import           Backend.Protocol.Types
import           Backend.Servant.Requests     (fromRelayResponse, getRelayUrlE,
                                               statusRequestWrapper)
import           Backend.Status               (Status (..))
import           Backend.Utility              (switchHoldDyn, toEither)
import           Backend.Wallet               (Wallet (..))
import           ENCOINS.Bulletproofs         (Secrets)
import           ENCOINS.Common.Widgets.Basic (btn, divClassId)
-- import           ENCOINS.Common.Events

sendRequestButtonWallet :: MonadWidget t m
  => EncoinsMode
  -> Dynamic t Status
  -> Dynamic t Wallet
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> Dynamic t [Text]
  -> m (Event t Status, Event t ())
sendRequestButtonWallet
  mode
  dStatus
  dWallet
  dCoinsToBurn
  dCoinsToMint
  e
  dUrls = mdo

    -- TODO: choose url every time by 'e' fires
    -- or every time user enters the tab.
    let eUrls = updated dUrls
    emUrl <- getRelayUrlE dUrls $ leftmost [() <$ eUrls, () <$ eRelayDown]
    let eAllRelayDown = filterLeft $ toEither () <$> emUrl
    dmUrl <- holdDyn Nothing emUrl

    eFireStatus <- delay 1 $ leftmost [e, () <$ eRelayDown]

    -- Getting current MaxAda
    eeStatus <- switchHoldDyn dmUrl $ \case
      Nothing  -> pure never
      Just url -> statusRequestWrapper url (pure MaxAdaWithdraw) eFireStatus
    let (eRelayDown, _, eMaxAda) = fromRelayResponse eeStatus

    let getMaxAda (MaxAdaWithdrawResult n) = Just n
        getMaxAda _                        = Nothing
    dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)

    -- SEND REQUEST button
    let dTxValidity = txValidityWallet mode
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

sendRequestButtonLedger :: MonadWidget t m
  => EncoinsMode
  -> Dynamic t Status
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> Dynamic t [Text]
  -> m (Event t Status, Event t ())
sendRequestButtonLedger mode dStatus dCoinsToBurn dCoinsToMint e dUrls = mdo

  let eUrls = updated dUrls
  -- TODO: choose url every time by 'e' fires
  -- or every time user enters the tab.
  emUrl <- getRelayUrlE dUrls $ leftmost [() <$ eUrls, () <$ eRelayDown]
  let eAllRelayDown = filterLeft $ toEither () <$> emUrl
  dmUrl <- holdDyn Nothing emUrl

  eFireStatus <- delay 1 $ leftmost [e, () <$ eRelayDown]

  -- Getting current MaxAda
  eeStatus <- switchHoldDyn dmUrl $ \case
    Nothing  -> pure never
    Just url -> statusRequestWrapper url (pure MaxAdaWithdraw) eFireStatus
  let (eRelayDown, _, eMaxAda) = fromRelayResponse eeStatus

  let getMaxAda (MaxAdaWithdrawResult n) = Just n
      getMaxAda _                        = Nothing
  dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)

  -- SEND REQUEST button
  let dTxValidity = txValidityLedger mode
        <$> dmUrl
        <*> dMaxAda
        <*> dStatus
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
