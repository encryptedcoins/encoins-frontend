module ENCOINS.App.Widgets.TransactionBalance where

import           Data.Bool                    (bool)
import           Data.Text                    (Text)
import           Reflex.Dom

import           ENCOINS.Common.Utils         (toText)
import           ENCOINS.Common.Widgets.Basic (column, divClassId, space)

transactionBalanceWidget :: MonadWidget t m
  => Dynamic t Integer
  -> Dynamic t Integer
  -> Text
  -> m ()
transactionBalanceWidget dBalance dFees txt = do
    let balanceSign bal
          | bal > 0 = "+"
          | bal < 0 = "-"
          | otherwise = ""
        balanceADA bal =
          "Transaction balance"
          <> txt
          <> column
          <> space
          <> balanceSign bal
          <> toText (abs bal)
          <> " ADA"
        feeADA f =
          let r = toText f
          in bool blank (divClass "app-text-semibold" $ text $ "Fee: " <> r <> " ADA") (r /= "0")
    divClassId "transaction-balance-div" "welcome-tx-balance" $ do
        divClass "app-text-semibold" $ dynText $ fmap balanceADA dBalance
        dyn_ $ fmap feeADA dFees
