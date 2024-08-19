module ENCOINS.DAO.Widgets.PollWidget where

import Control.Lens ((^.))
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( UTCTime
    )
import qualified Foreign.JavaScript.Utils as Utils
import GHCJS.DOM.Blob (newBlob)
import qualified GHCJS.DOM.Document as D
import GHCJS.DOM.Element (setAttribute)
import qualified GHCJS.DOM.HTMLElement as DOMHtml
import GHCJS.DOM.Types hiding (ByteString, Event, Text, toText)
import GHCJS.DOM.URL (createObjectURL, revokeObjectURL)
import qualified Language.Javascript.JSaddle as JS
import Reflex.Dom
import Text.Printf (printf)

import Backend.Wallet (LucidConfig (..), Wallet (..), lucidConfigDao, toJS)
import Common.Reflex.Dom.Extra (elementResultJS, textLocale)
import Common.Utility (formatPollTime, toJsonStrict, toText)
import ENCOINS.Common.Widgets.Basic (btn, btnWithBlock)
import ENCOINS.DAO.Widgets.Poll.PollResults (VoteResult (..))
import ENCOINS.DAO.Widgets.Poll.Polls (Poll (..))
import ENCOINS.Website.Widgets.Basic (container)
import qualified I18n.Dao as I18n
import I18n.I18n (App)
import JS.DAO (daoPollVoteTx)

pollWidget ::
    (App t m) =>
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

pollCompletedWidget :: (App t m) => Poll m -> m ()
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
        $ textLocale I18n.DownloadResults
    eDownload <- container "" $
        divClass "dao-VoteDownload" $ do
            btn "button-switching flex-center" "" $ textLocale I18n.Download

    downloadVotes (toJsonStrict voteResults) "result" n eDownload
    downloadVotes (encodeUtf8 fullAnswers) "result_full" n eDownload

mkVoteList :: VoteResult -> [(Text, Text)]
mkVoteList (VoteResult yes no) =
    [ ("Yes", pack $ printf "%.2f%%" yes)
    , ("No", pack $ printf "%.2f%%" no)
    ]

viewPollExplainer :: (App t m) => m () -> m () -> UTCTime -> m ()
viewPollExplainer tagsTitle tagsExplainer endTime = container "" $
    divClass "div-explainer" $ do
        elAttr "h4" ("class" =: "h4" <> "style" =: "margin-bottom: 30px;") tagsTitle
        elAttr
            "p"
            ("class" =: "p-explainer" <> "style" =: "text-align: justify;")
            tagsExplainer
        divClass "app-text-small" $ do
            textLocale I18n.EndDate
            text $ formatPollTime endTime <> "."

triggerDownload ::
    (MonadJSM m) =>
    Document
    -> Text
    -- ^ mime type
    -> Text
    -- ^ file name
    -> ByteString
    -- ^ content
    -> m ()
triggerDownload doc mime filename s = do
    t <- Utils.bsToArrayBuffer s
    o <- JS.liftJSM $ JS.obj ^. JS.jss ("type" :: Text) (mime :: Text)
    options <- JS.liftJSM $ BlobPropertyBag <$> JS.toJSVal o
    blob <- newBlob [t] (Just options)
    (url :: Text) <- createObjectURL blob
    a <- D.createElement doc ("a" :: Text)
    setAttribute a ("style" :: Text) ("display: none;" :: Text)
    setAttribute a ("download" :: Text) filename
    setAttribute a ("href" :: Text) url
    DOMHtml.click $ DOMHtml.HTMLElement $ unElement a
    revokeObjectURL url

downloadVotes ::
    (MonadWidget t m) => ByteString -> Text -> Int -> Event t () -> m ()
downloadVotes txt name num e = do
    doc <- askDocument
    performEvent_ $ ffor e $ \_ ->
        triggerDownload doc "application/json" (name <> toText num <> ".json") txt
