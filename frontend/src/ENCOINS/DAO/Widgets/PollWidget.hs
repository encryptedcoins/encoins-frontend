module ENCOINS.DAO.Widgets.PollWidget where

import           Reflex.Dom

import           Backend.Wallet                (Wallet (..), toJS, lucidConfigDao)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.Common.Utils          (toText)
import           ENCOINS.Common.Widgets.Basic  (btn, btnWithEnterBlock)
import           ENCOINS.DAO.Polls             (Poll (..), formatPollTime)
import           ENCOINS.Website.Widgets.Basic (container)
import           JS.DAO                        (daoPollVoteTx)


pollWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Bool
  -> Poll m
  -> m ()
pollWidget dWallet dIsBlocked (Poll n question summary answers' endTime) = do
  explainer question summary

  let answers = fmap fst answers'
  container "" $ do
    es <- mapM
      (btnWithEnterBlock
        "button-switching .dao-Poll_Button"
      -- (btnWithBlock
        -- "button-switching"
        -- "margin-left: 30px; margin-right: 30px; margin-bottom: 20px;"
        dIsBlocked . text
      ) answers
    let e = leftmost $ zipWith (<$) answers es

    performEvent_
      $ daoPollVoteTx n lucidConfigDao
      <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) e

    dMsg <- elementResultJS ("elementPoll" <> toText n) id
    container "" $ divClass "app-text-normal" $ dynText dMsg
  where
    -- TODO: make this a widget
    explainer tagsTitle tagsExplainer = container "" $ divClass "div-explainer" $ do
      elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") tagsTitle
      elAttr "p" ("class" =: "p-explainer" <> "style" =: "text-align: justify;") tagsExplainer
      divClass "app-text-small" $ text $ "The vote ends on " <> formatPollTime endTime <> "."

pollCompletedWidget :: MonadWidget t m => Poll m -> m ()
pollCompletedWidget (Poll _ question summary answers' endTime) = do
  explainer question summary

  container "" $
    mapM_ (\(a, r) -> btn "vote-option-result" "margin-left: 30px; margin-right: 30px; margin-bottom: 20px;" $ do
      text a
      elAttr "div" ("style" =: "margin-right: 10px; margin-left: 10px;") blank
      text r
      ) answers'
  where
    -- TODO: make this a widget
    explainer tagsTitle tagsExplainer = container "" $ divClass "div-explainer" $ do
      elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") tagsTitle
      elAttr "p" ("class" =: "p-explainer" <> "style" =: "text-align: justify;") tagsExplainer
      divClass "app-text-small" $ text $ "The vote ended on " <> formatPollTime endTime <> "."
