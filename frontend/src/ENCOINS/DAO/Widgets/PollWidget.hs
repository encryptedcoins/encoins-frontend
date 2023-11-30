module ENCOINS.DAO.Widgets.PollWidget where

import           Data.Text                     (Text, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Reflex.Dom
import           Text.Printf

import           Backend.Wallet                (LucidConfig (..), Wallet (..),
                                                lucidConfigDao, toJS)
import           ENCOINS.App.Widgets.Basic     (elementResultJS)
import           ENCOINS.Common.Utils          (downloadVotes, toText)
import           ENCOINS.Common.Widgets.Basic  (btn, btnWithBlock)
import           ENCOINS.DAO.PollResults
import           ENCOINS.DAO.Polls             (Poll (..), formatPollTime)
import           ENCOINS.Website.Widgets.Basic (container)
import           JS.DAO                        (daoPollVoteTx)


pollWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Bool
  -> Poll m
  -> m ()
pollWidget dWallet dIsBlocked (Poll n question summary answers' _ endTime) = do
  explainer question summary

  let answers = fmap fst $ mkVoteList answers'
  container "" $ do
    es <- mapM
      (btnWithBlock
        "button-switching dao-Poll_Button"
        ""
        dIsBlocked . text
      ) answers
    let e = leftmost $ zipWith (<$) answers es
    let LucidConfig apiKey networkId _ _ asset = lucidConfigDao
    performEvent_
      $ daoPollVoteTx n apiKey networkId asset
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
pollCompletedWidget (Poll n question summary voteResults fullAnswers endTime) = do
  explainer question summary

  container "" $
    mapM_ (\(a, r) -> btn "vote-option-result" "margin-left: 30px; margin-right: 30px; margin-bottom: 20px;" $ do
      text a
      elAttr "div" ("style" =: "margin-right: 10px; margin-left: 10px;") blank
      text r
      ) $ mkVoteList voteResults
  container ""
    $ elAttr "div" ("class" =: "h5" <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);") $ text "Download poll results"
  container "" $
    divClass "dao-VoteDownload" $ do
      eDownload <- btn "button-switching flex-center" "" $ text "DOWNLOAD"
      downloadVotes (toJsonResult voteResults) "result" n eDownload
      downloadVotes (encodeUtf8 fullAnswers) "result_full" n eDownload
  where
    -- TODO: make this a widget
    explainer tagsTitle tagsExplainer = container "" $ divClass "div-explainer" $ do
      elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") tagsTitle
      elAttr "p" ("class" =: "p-explainer" <> "style" =: "text-align: justify;") tagsExplainer
      divClass "app-text-small" $ text $ "The vote ended on " <> formatPollTime endTime <> "."

mkVoteList :: VoteResult -> [(Text, Text)]
mkVoteList (VoteResult yes no) =
  [ ("Yes", pack $ printf "%.2f%%" yes)
  , ("No", pack $ printf "%.2f%%" no)]
