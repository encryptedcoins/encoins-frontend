module ENCOINS.App.Widgets.SendRequestButton where

import           Control.Monad                          (when, void)
import           Data.Maybe                             (isNothing)
import           Reflex.Dom

import           Backend.Servant.Requests               (statusRequestWrapper)
import           Backend.Servant.Client                 (getRelayUrl)
import           Backend.Status                         (Status(..))
import           Backend.Types
import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Protocol.TxValidity        (TxValidity(..), txValidity)
import           ENCOINS.Bulletproofs                   (Secrets)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           JS.Website                             (setElementStyle)

sendRequestButton :: MonadWidget t m => EncoinsMode -> Dynamic t Integer -> Dynamic t Status -> Dynamic t Wallet ->
  Dynamic t Secrets -> Dynamic t Secrets -> m (Event t ())
sendRequestButton mode dBalance dStatus dWallet dCoinsToBurn dCoinsToMint = do
  -- Getting the current MaxAda
  mbaseUrl <- getRelayUrl
  when (isNothing mbaseUrl) $
    setElementStyle "bottom-notification-relay" "display" "flex"
  (eMaxAda, _) <- case mbaseUrl of
    Just baseUrl -> statusRequestWrapper baseUrl (pure MaxAdaWithdraw)
      (void $ updated dBalance)
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
          TxValid     -> blank
          TxInvalid e -> elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
              "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $
              divClass "app-text-normal" $ text e
      h v = case v of
        TxValid -> ""
        _       -> "border-bottom-left-radius: 0px; border-bottom-right-radius: 0px"
  e <- divClassId "" "welcome-send-req" $ btn (fmap f dTxValidity) (fmap h dTxValidity) $ dynText "SEND REQUEST"
  dyn_ $ fmap g dTxValidity
  return $ () <$ ffilter (== TxValid) (current dTxValidity `tag` e)
