module ENCOINS.DAO.Widgets.PollWidget where

import           Reflex.Dom

import           Backend.Wallet                (Wallet (..), toJS)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.Common.Utils          (toText)
import           ENCOINS.Common.Widgets.Basic  (pClass, btn)
import           ENCOINS.DAO.Polls             (Poll (..))
import           ENCOINS.Website.Widgets.Basic (container, section)
import           JS.DAO                        (daoPollVoteTx)

pollWidget :: MonadWidget t m => Poll -> Dynamic t Wallet -> m ()
pollWidget (Poll n question summary answers endTime) dWallet = section "" "" $ do
  explainer question summary

  container "" $ do
    es <- mapM (btn "" "margin-left: 30px; margin-right: 30px; margin-bottom: 20px;" . text) answers
    let e = leftmost $ zipWith (<$) answers es

    performEvent_ $ daoPollVoteTx n <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) e

    dMsg <- elementResultJS ("elementPoll" <> toText n) id
    container "" $ divClass "app-text-normal" $ dynText dMsg

    blank
  where
    -- TODO: make this a widget
    explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
      elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") $ text txtTitle
      pClass "p-explainer" $ text txtExplainer
      divClass "app-text-small" $ text $ "Voting ends on " <> endTime <> "."