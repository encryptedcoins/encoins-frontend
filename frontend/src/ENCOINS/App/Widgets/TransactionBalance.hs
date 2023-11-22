module ENCOINS.App.Widgets.TransactionBalance where

import           Data.Bool                       (bool)
import           Data.Text                       (Text)
import           Reflex.Dom

import           Backend.Protocol.Types          (EncoinsMode (..))
import           ENCOINS.Common.Utils            (toText)
import           ENCOINS.Common.Widgets.Basic    (br, column, divClassId, image,
                                                  space)

data Formula t = Formula
  { total    :: Dynamic t Integer
  , fee      :: Dynamic t Integer
  , bAda     :: Dynamic t Integer
  , mAda     :: Dynamic t Integer
  , bEncoins :: Dynamic t Integer
  , mEncoins :: Dynamic t Integer
  }

transactionBalanceWidget :: MonadWidget t m
  => Formula t
  -> Maybe EncoinsMode
  -> Text
  -> m ()
transactionBalanceWidget formula mMode txt = do
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
  case mMode of
    Nothing -> divClassId "app-TransactionBalance" "welcome-tx-balance" $
      divClass "app-text-semibold" $ dynText $ fmap balanceADA $ total formula
    Just mode -> do
      ev <- divClassId "app-TransactionBalance" "welcome-tx-balance" $ do
          ev <- image "info.svg" "app-Formula_InfoIcon" ""
          divClass "app-text-semibold" $ dynText $ fmap balanceADA $ total formula
          pure ev
      dTooltipVis <- toggle False ev
      dyn_ $ bool blank (formulaTooltip formula mode) <$> dTooltipVis

formulaTooltip :: MonadWidget t m => Formula t -> EncoinsMode -> m ()
formulaTooltip Formula{..} mode = elAttr "div"
  ( "class" =: "app-Formula_TooltipWrapper"
  <> "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px"
  ) $ do
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "font-size:16px;overflow-wrap: anywhere;") $ case mode of
        WalletMode -> divClass "app-Formula_TooltipFormula" $ do
            dynText $ mconcat
              [ (toText <$> total)
              , " = ("
              , (toText <$> bAda)
              , " - "
              , (toText <$> mAda)
              , ") - "
              , (toText <$> fee)
              ]
            br
            text "txBalance = (bAda - mAda) - fee"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_TooltipLegend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda = sum of Ada in the encoins being burned"
                    , "mAda = sum of Ada in the encoins being minted"
                    , "fee = commission of the relay"
                    ]
        TransferMode -> divClass "app-Formula_TooltipFormula" $ do
            dynText $ mconcat
              [ (toText <$> total)
              , " = - ("
              , (toText <$> bEncoins)
              , " * 4)"
              ]
            br
            text "txBalance = - (nEncoins * deposit)"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_TooltipLegend ") $ do
                mapM_ (el "li" . text)
                    [ "nEncoins = number of the encoins being transferred"
                    , "deposit = returnable deposit for placing your encoins into ledger (4 Ada)"
                    ]
        LedgerMode -> divClass "app-Formula_TooltipFormula" $ do
            dynText $ mconcat
              [ (toText <$> total)
              , " = ("
              , (toText <$> bAda)
              , " - "
              , (toText <$> mAda)
              , ") + ("
              , (toText <$> bEncoins)
              , " - "
              , (toText <$> mEncoins)
              , ") * 4 - "
              , (toText <$> fee)
              ]
            br
            text "txBalance = (bAda - mAda) + (bEncoins - mEncoins) * deposit - fee"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_TooltipLegend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda = sum of Ada in the encoins being burned"
                    , "mAda = sum of Ada in the encoins being minted"
                    , "bEncoins = number of the encoins being burned"
                    , "mEncoins = number of the encoins being minted"
                    , "deposit = returnable deposit for placing your encoins into the ledger (4 Ada)"
                    , "fee = commission of the relay"
                    ]
