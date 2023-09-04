module ENCOINS.App.Widgets.TransactionBalance where

import           Data.Bool                        (bool)
import           Reflex.Dom

import           ENCOINS.Common.Utils             (toText)
import           ENCOINS.Common.Widgets.Basic     (divClassId)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Integer -> Dynamic t Integer -> m ()
transactionBalanceWidget dBalance dFees =
    let balanceSign bal = bool "+" "-" (bal > 0)
        balanceADA bal = "Transaction balance: " <> balanceSign bal <> toText (abs bal) <> " ADA"
        feeADA f =
          let r = toText f
          in bool blank (divClass "app-text-semibold" $ text $ "Fee: " <> r <> " ADA") (r /= "0")
    in divClassId "transaction-balance-div" "welcome-tx-balance" $ do
        divClass "app-text-semibold" $ dynText $ fmap balanceADA dBalance
        dyn_ $ fmap feeADA dFees