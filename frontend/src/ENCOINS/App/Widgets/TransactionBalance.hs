{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.TransactionBalance where

import Data.Bool (bool)
import qualified Data.Text as T
import Reflex.Dom

import Backend.Protocol.Types (EncoinsMode (..))
import Common.Utility (column, space, toText)

import Common.Reflex.Dom.Extra
import ENCOINS.Common.Widgets.Basic (br, divClassId, image)
import qualified I18n.App as I18n
import I18n.I18n (App)
import qualified I18n.Reflex.I18n as I18n

data Formula t = Formula
    { total :: Dynamic t Integer
    , fee :: Dynamic t Integer
    , bAda :: Dynamic t Integer
    , mAda :: Dynamic t Integer
    , bEncoins :: Dynamic t Integer
    , mEncoins :: Dynamic t Integer
    }

transactionBalanceWidget ::
    (App t m) =>
    Formula t
    -> Maybe EncoinsMode
    -> Maybe I18n.AppMessage
    -> m ()
transactionBalanceWidget formula mMode mTargetTerm = do
    let balanceSign bal
            | bal > 0 = "+"
            | bal < 0 = "-"
            | otherwise = ""
        balanceADA title mTarget bal =
            let target = maybe T.empty (\t -> "(" <> t <> ")") mTarget
            in title
                <> space
                <> target
                <> column
                <> space
                <> balanceSign bal
                <> toText (abs bal)
                <> " ADA"
    dBalanceTitle <- I18n.showLocale I18n.Balance
    dmTarget <- case mTargetTerm of 
        Just t -> fmap Just <$> I18n.showLocale t
        Nothing -> pure $ constDyn Nothing
    let dBalanceAda = balanceADA <$> dBalanceTitle <*> dmTarget <*> total formula
    case mMode of
        Nothing ->
            divClassId "app-TransactionBalance" "welcome-tx-balance" $
                divClass "app-text-semibold" $
                    dynText dBalanceAda
        Just mode -> mdo
            ev <- divClassId "app-TransactionBalance" "welcome-tx-balance" $ do
                divClass "app-text-semibold" $ dynText dBalanceAda
                let arrowClass =
                        bool "app-Spoiler_Formula-down" "app-Spoiler_Formula-up" <$> dIsTooltipVisible
                image "arrow_down.svg" arrowClass ""
            dIsTooltipVisible <- toggle False ev
            dyn_ $ bool blank (formulaTooltip formula mode) <$> dIsTooltipVisible

formulaTooltip :: (App t m) => Formula t -> EncoinsMode -> m ()
formulaTooltip Formula{..} mode = divClass "app-Formula_TooltipWrapper" $
    do
        divClass "app-text-semibold" $ textLocale I18n.BalanceFormula
        elAttr
            "div"
            ( "class" =: "app-text-normal"
                <> "style" =: "font-size:16px;overflow-wrap: anywhere;"
            )
            $ case mode of
                WalletMode -> divClass "app-Formula_TooltipFormula" $ do
                    dynText $
                        mconcat
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
                        dBurned <- I18n.showLocale I18n.BalanceBurnAda
                        dMinted <- I18n.showLocale I18n.BalanceMintAda
                        dFee <- I18n.showLocale I18n.BalanceFee
                        dCommission <- I18n.showLocale I18n.BalanceCommission
                        mapM_
                            (el "li" . dynText)
                            [ T.append "bAda = " <$> dBurned
                            , T.append "mAda = " <$> dMinted
                            , mconcat [dFee, constDyn " = ", dCommission]
                            ]
                TransferMode -> divClass "app-Formula_TooltipFormula" $ do
                    dynText $
                        mconcat
                            [ (toText <$> total)
                            , " = - ("
                            , (toText <$> bEncoins)
                            , " * 4)"
                            ]
                    br
                    text "txBalance = - (nEncoins * deposit)"
                    elAttr "ul" ("role" =: "list" <> "class" =: "app-Formula_TooltipLegend ") $ do
                        dBalanceEncoins <- I18n.showLocale I18n.BalanceNumberEncoins
                        dBalanceDeposit <- I18n.showLocale I18n.BalanceDeposit
                        mapM_
                            (el "li" . dynText)
                            [ T.append "nEncoins = " <$> dBalanceEncoins
                            , T.append "deposit = " <$> dBalanceDeposit
                            ]
                LedgerMode -> divClass "app-Formula_TooltipFormula" $ do
                    dynText $
                        mconcat
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
                        dBurned <- I18n.showLocale I18n.BalanceBurnAda
                        dMinted <- I18n.showLocale I18n.BalanceMintAda
                        dBurnedNumber <- I18n.showLocale I18n.BalanceBurnEncoins
                        dMintedNumber <- I18n.showLocale I18n.BalanceMintEncoins
                        dBalanceDeposit <- I18n.showLocale I18n.BalanceDeposit
                        dFee <- I18n.showLocale I18n.BalanceFee
                        dCommission <- I18n.showLocale I18n.BalanceCommission
                        mapM_
                            (el "li" . dynText)
                            [ T.append "bAda = " <$> dBurned
                            , T.append "mAda = " <$> dMinted
                            , T.append "bEncoins = " <$> dBurnedNumber
                            , T.append "mEncoins = " <$> dMintedNumber
                            , T.append "deposit = " <$> dBalanceDeposit
                            , mconcat [dFee, constDyn " = ", dCommission]
                            ]
