{-# LANGUAGE QuasiQuotes #-}

module ENCOINS.Common.Utils where

import           Control.Lens                ((^.))
import           Control.Monad               (guard)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Foreign.JavaScript.Utils    as Utils
import           GHCJS.DOM.Blob
import qualified GHCJS.DOM.Document          as D
import           GHCJS.DOM.Element
import qualified GHCJS.DOM.HTMLElement       as DOMHtml
import           GHCJS.DOM.Types             hiding (ByteString, Event, Text,
                                              toText)
import           GHCJS.DOM.URL
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom
import           Text.RawString.QQ           (r)
import           Text.Regex.TDFA             (CompOption (lastStarGreedy),
                                              ExecOption (captureGroups), Regex,
                                              defaultCompOpt, defaultExecOpt,
                                              matchTest)
import           Text.Regex.TDFA.Text        (compile)

toText :: Show a => a -> Text
toText = pack . show

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _      = Nothing
    go  (x:_) 0  = Just x
    go  (_:xs) i = go xs (pred i)

normalizePingUrl :: Text -> Text
normalizePingUrl t = T.append (T.dropWhileEnd (== '/') t) "//"

checkUrl :: Text -> Bool
checkUrl = regexPosixOpt urlRegexPosixPattern

regexPosixOpt :: Text -> Text -> Bool
regexPosixOpt rPattern input = matchTest rOpt input
  where
    rOpt :: Regex
    rOpt = case reg of
      Left err    -> error err
      Right regex -> regex
    reg = compile
      defaultCompOpt{lastStarGreedy=True}
      defaultExecOpt{captureGroups=False}
      rPattern

urlRegexPosixPattern :: Text
urlRegexPosixPattern = [r|^https?://((([1-9][[:digit:]]?|1[[:digit:]][[:digit:]]|2[01][[:digit:]]|22[0-3])(\.(1?[[:digit:]]{1,2}|2[0-4][[:digit:]]|25[0-5])){2}(\.([1-9][[:digit:]]?|1[[:digit:]][[:digit:]]|2[0-4]|25[0-4])))|((([[:alnum:]]-[[:alnum:]]{0,62})?[[:alnum:]]+\.)+([[:alpha:]]{2,})))(:[[:digit:]]{2,5})?(/[0-9a-zA-Z$-_.+!*'(),%?&=:@/;~#]*)?$|]

triggerDownload
  :: MonadJSM m
  => Document
  -> Text -- ^ mime type
  -> Text -- ^ file name
  -> ByteString -- ^ content
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

downloadVotes :: MonadWidget t m => ByteString -> Text -> Int -> Event t () -> m ()
downloadVotes txt name num e = do
  doc <- askDocument
  performEvent_ $ ffor e $ \_ ->
    triggerDownload doc "application/json" (name <> toText num <> ".json") txt
