module ENCOINS.DAO.Widgets.PollWidget where

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( UTCTime
    )
import Reflex.Dom
import Text.Printf

import Backend.Utility (formatPollTime, toText)
import Backend.Wallet (LucidConfig (..), Wallet (..), lucidConfigDao, toJS)
import ENCOINS.App.Widgets.Basic (elementResultJS)
import ENCOINS.Common.Utils (downloadVotes, toJsonStrict)
import ENCOINS.Common.Widgets.Basic (btn, btnWithBlock)
import ENCOINS.DAO.Widgets.Poll.PollResults
import ENCOINS.DAO.Widgets.Poll.Polls (Poll (..))
import ENCOINS.Website.Widgets.Basic (container)
import JS.DAO (daoPollVoteTx)

pollWidget ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t Bool
    -> Poll m
    -> m ()
pollWidget dWallet dIsBlocked (Poll n question summary answers' _ endTime) = do
    viewPollExplainer question summary endTime
    let answers = fmap fst $ mkVoteList answers'
    eAnswers <- do
        let viewPollButton answer =
                btnWithBlock
                    "button-switching dao-Poll_Button"
                    ""
                    dIsBlocked
                    $ text answer
        container "" $ mapM viewPollButton answers
    let eAnswer = leftmost $ zipWith (<$) answers eAnswers
    let LucidConfig apiKey networkId policyId assetName = lucidConfigDao
    performEvent_ $
        daoPollVoteTx n apiKey networkId policyId assetName
            <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eAnswer

    dMsg <- elementResultJS ("elementPoll" <> toText n) id
    container "" $ divClass "app-text-normal" $ dynText dMsg

pollCompletedWidget :: (MonadWidget t m) => Poll m -> m ()
pollCompletedWidget (Poll n question summary voteResults fullAnswers endTime) = do
    viewPollExplainer question summary endTime

    container ""
        $ mapM_
            ( \(a, r) -> btn
                "vote-option-result"
                "margin-left: 30px; margin-right: 30px; margin-bottom: 20px;"
                $ do
                    text a
                    elAttr "div" ("style" =: "margin-right: 10px; margin-left: 10px;") blank
                    text r
            )
        $ mkVoteList voteResults
    container ""
        $ elAttr
            "div"
            ( "class" =: "h5"
                <> "style" =: "-webkit-filter: brightness(35%); filter: brightness(35%);"
            )
        $ text "Download poll results"
    eDownload <- container "" $
        divClass "dao-VoteDownload" $ do
            btn "button-switching flex-center" "" $ text "DOWNLOAD"

    downloadVotes (toJsonStrict voteResults) "result" n eDownload
    downloadVotes (encodeUtf8 fullAnswers) "result_full" n eDownload

mkVoteList :: VoteResult -> [(Text, Text)]
mkVoteList (VoteResult yes no) =
    [ ("Yes", pack $ printf "%.2f%%" yes)
    , ("No", pack $ printf "%.2f%%" no)
    ]

viewPollExplainer :: (MonadWidget t m) => m () -> m () -> UTCTime -> m ()
viewPollExplainer tagsTitle tagsExplainer endTime = container "" $
    divClass "div-explainer" $ do
        elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") tagsTitle
        elAttr
            "p"
            ("class" =: "p-explainer" <> "style" =: "text-align: justify;")
            tagsExplainer
        divClass "app-text-small" $
            text $
                "The vote ends on " <> formatPollTime endTime <> "."
