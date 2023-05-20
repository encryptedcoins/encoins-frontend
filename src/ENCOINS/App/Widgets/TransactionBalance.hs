{-# LANGUAGE NumericUnderscores #-}

module ENCOINS.App.Widgets.TransactionBalance where

import           Data.Bool                        (bool)
import           Data.Text                        (Text, pack)
import           PlutusTx.Prelude                 (divide)
import           Reflex.Dom
import           Text.Printf                      (printf)

import           Backend.Types
import           ENCOINS.Common.Widgets.Basic     (divClassId)
import           Widgets.Utils                    (toText)

protocolFee :: EncoinsMode -> Integer -> Integer
protocolFee mode n
    | n > 0 || mode == LedgerMode = (*2) . max f $ (n * 1_000_000) `divide` 100
    | otherwise                   = 0
    where f = case mode of
            WalletMode -> 1_500_000
            LedgerMode -> 2_000_000
            _          -> 0

formatRatio :: Double -> String
formatRatio = printf "%.2f"

calculateRatio :: Integer -> Integer -> Text
calculateRatio x y = pack $ formatRatio $ fromIntegral x / fromIntegral y

transactionBalanceWidget :: MonadWidget t m => EncoinsMode -> Dynamic t Integer -> m ()
transactionBalanceWidget mode dBalance =
    let balanceSign n = bool "-" "+" (n >= 0)
        balanceADA n = "Transaction balance: " <> balanceSign n <> toText (abs n) <> " ADA"
        feeADA n =
          let r = calculateRatio (protocolFee mode n) 1_000_000
          in bool blank (divClass "app-text-semibold" $ text $ "Fee: " <> r <> " ADA") (r /= "0.00")
    in divClassId "transaction-balance-div" "welcome-tx-balance" $ do
        divClass "app-text-semibold" $ dynText $ fmap balanceADA dBalance
        dyn_ $ fmap feeADA dBalance