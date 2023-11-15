module ENCOINS.App.Widgets.TransactionBalance where

import           Data.Bool                       (bool)
import           Data.Text                       (Text)
import           Reflex.Dom

import           Backend.Protocol.Types          (EncoinsMode (..))
import           ENCOINS.Common.Utils            (toText)
import           ENCOINS.Common.Widgets.Advanced (withTooltip)
import           ENCOINS.Common.Widgets.Basic    (column, divClassId, space)

transactionBalanceWidget :: MonadWidget t m
  => Dynamic t Integer
  -> Dynamic t Integer
  -> Maybe EncoinsMode
  -> Text
  -> m ()
transactionBalanceWidget dBalance dFees mMode txt = do
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
  (elTxt,_) <- withTooltip
    (divClassId "transaction-balance-div" "welcome-tx-balance" $ elClass' "div" "app-text-semibold" $ do
          dynText $ fmap balanceADA dBalance
          dyn_ $ fmap feeADA dFees) mempty 0 0 $ do
            elAttr "div" ("class" =: "app-text-semibold app-Formula_Popup") $ text "Formula"
  dTooltipVis <- toggle False (domEvent Click elTxt)
  dyn_ $ bool blank (formulaTooltip mMode) <$> dTooltipVis


      -- divClass "app-text-semibold" $ dynText $ fmap balanceADA dBalance
      -- dyn_ $ fmap feeADA dFees


formulaTooltip :: MonadWidget t m => Maybe EncoinsMode -> m ()
formulaTooltip mode = elAttr "div"
  ( "class" =: "div-tooltip div-tooltip-always-visible"
  <> "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px"
  ) $ do
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "font-size:16px;overflow-wrap: anywhere;") $ case mode of
        Just WalletMode -> do
            text "txBalance = (bAda - mAda) - fee"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda - sum Ada in burning encoins"
                    , "mAda - sum Ada in minting encoins"
                    , "fee - commission of relay"
                    ]
        Just TransferMode -> divClass "app-Formula_Tooltip" $ do
        -- Just TransferMode -> do
            text "txBalance = eN * 4"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "eN - Number of transferring encoins"
                    ]
        -- Just LedgerMode -> divClass "app-Formula_Tooltip" $ do
        Just LedgerMode -> do
            text "txBalance = (bAda - mAda) + (bN - mN) - fee"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda - sum Ada in burning encoins"
                    , "mAda - sum Ada in minting encoins"
                    , "bN - Number of burning encoins"
                    , "mN - Number of minting encoins"
                    , "fee - commission of relay"
                    ]
        Nothing -> blank
