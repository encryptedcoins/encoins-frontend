module ENCOINS.App.Widgets.SendRequestButton where

import           Reflex.Dom
import           Data.Text                              (Text)

import           Backend.Protocol.TxValidity            (TxValidity(..), txValidity)
import           Backend.Protocol.Types
import           Backend.Servant.Requests               (statusRequestWrapper)
import           Backend.Servant.Client                 (getRelayUrl)
import           Backend.Status                         (Status(..))
import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.Bulletproofs                   (Secrets)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           ENCOINS.App.Widgets.Basic              (relayStatusM)

sendRequestButton :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => EncoinsMode
  -> Dynamic t Status
  -> Dynamic t Wallet
  -> Dynamic t Secrets
  -> Dynamic t Secrets
  -> Event t ()
  -> m (Event t ())
sendRequestButton mode dStatus dWallet dCoinsToBurn dCoinsToMint e = do
  -- Getting the current MaxAda
  mbaseUrl <- getRelayUrl
  relayStatusM mbaseUrl
  (eMaxAda, _) <- case mbaseUrl of
    Just baseUrl -> statusRequestWrapper baseUrl (pure MaxAdaWithdraw) e
    _ -> pure (never, never)
  let getMaxAda (MaxAdaWithdrawResult n) = Just n
      getMaxAda _ = Nothing
  dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)
  -- SEND REQUEST button
  let dTxValidity = txValidity mbaseUrl mode <$> dMaxAda <*> dStatus <*> dWallet <*> dCoinsToBurn <*> dCoinsToMint
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