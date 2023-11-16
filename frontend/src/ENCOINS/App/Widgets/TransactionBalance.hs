module ENCOINS.App.Widgets.TransactionBalance where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import           Data.Text                       (Text)
import           Reflex.Dom

import           Backend.Protocol.Types          (EncoinsMode (..))
import           ENCOINS.Common.Utils            (toText)
import           ENCOINS.Common.Widgets.Advanced (withTooltip)
import           ENCOINS.Common.Widgets.Basic    (br, column, divClassId, space)

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
  let txBalance =
        divClassId "transaction-balance-div" "welcome-tx-balance" $
          elClass' "div" "app-text-semibold" $ dynText $ fmap balanceADA $ total formula
  case mMode of
    Nothing -> void txBalance
    Just mode -> do
      (elTxt,_) <- withTooltip txBalance mempty 0 0 $ do
          elAttr "div" ("class" =: "app-text-semibold app-Formula_Popup") $ text "Formula"
      dTooltipVis <- toggle False (domEvent Click elTxt)
      dyn_ $ bool blank (formulaTooltip formula mode) <$> dTooltipVis

formulaTooltip :: MonadWidget t m => Formula t -> EncoinsMode -> m ()
formulaTooltip Formula{..} mode = elAttr "div"
  ( "class" =: "div-tooltip div-tooltip-always-visible"
  <> "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px"
  ) $ do
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "font-size:16px;overflow-wrap: anywhere;") $ case mode of
        WalletMode -> divClass "app-Formula_TooltipWrapper" $ do
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
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda - sum Ada in burning encoins"
                    , "mAda - sum Ada in minting encoins"
                    , "fee - commission of relay"
                    ]
        TransferMode -> divClass "app-Formula_TooltipWrapper" $ do
            dynText $ mconcat
              [ (toText <$> total)
              , " = - ("
              , (toText <$> bEncoins)
              , " * 4)"]
            br
            text "txBalance = - (nEncoins * 4)"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "nEncoins - number of transferring encoins"
                    ]
        LedgerMode -> divClass "app-Formula_TooltipWrapper" $ do
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
              , ") - "
              , (toText <$> fee)
              ]
            br
            text "txBalance = (bAda - mAda) + (bEncoins - mEncoins) - fee"
            elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_Tooltip-legend ") $ do
                mapM_ (el "li" . text)
                    [ "bAda - sum Ada in burning encoins"
                    , "mAda - sum Ada in minting encoins"
                    , "bEncoins - number of burning encoins"
                    , "mEncoins - number of minting encoins"
                    , "fee - commission of relay"
                    ]
