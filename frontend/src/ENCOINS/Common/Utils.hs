{-# LANGUAGE QuasiQuotes #-}

module ENCOINS.Common.Utils where

import           Backend.Utility             (toText)

import           Control.Lens                ((^.))
import           Control.Monad               (guard)
import           Data.Aeson                  (ToJSON, encode)
import           Data.Attoparsec.Text        (Parser, char, choice, parseOnly,
                                              sepBy1, takeWhile1, (<?>))
import qualified Data.Attoparsec.Text        as A
import           Data.ByteString             (ByteString)
import           Data.ByteString.Lazy        (toStrict)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
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

toJsonText :: ToJSON a => a -> Text
toJsonText = decodeUtf8 . toJsonStrict

toJsonStrict :: ToJSON a => a -> ByteString
toJsonStrict = toStrict . encode

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _      = Nothing
    go  (x:_) 0  = Just x
    go  (_:xs) i = go xs (pred i)

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
urlRegexPosixPattern = [r|^https?://((25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)|(([[:alnum:]]+|([[:alnum:]]+\-[[:alnum:]]*)*[[:alnum:]])(\.([[:alnum:]]+|([[:alnum:]]+\-[[:alnum:]]*)*[[:alnum:]]))*\.([[:alpha:]]{2,})))/$|]

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

--------------------------------------------------------------------------------
-- Remove prefixes 'http(s):// and suffixes '/', ':' and further symbols rom URL
--------------------------------------------------------------------------------

-- As stripHost but return unstriped relay on stripping fails
stripHostOrRelay :: Text -> Text
stripHostOrRelay u = either (const u) id $ stripHost u

stripHost :: Text -> Either String Text
stripHost = fmap printHost . parseOnly stripFixes

stripFixes :: Parser Host
stripFixes = do
  _ <- A.option "" $ choice [A.string "https://", A.string "http://"]
  parseHost

data Host
  = Localhost
  | N NormalHost

data NormalHost = NormalHost
      { uriHostName    :: !(Vector Text)
      , urihHostSuffix :: !Text
      }

printHost :: Host -> Text
printHost x = case x of
      Localhost           -> "localhost"
      N (NormalHost ns c) -> T.intercalate "." (V.toList (ns `V.snoc` c))

parseHost :: Parser Host
parseHost = do
  let hostChunk = takeWhile1 (\c -> c `notElem` ['.',':','/','?','#']) <?> "host chunk"
      hostChunks = hostChunk `sepBy1` char '.' <?> "host chunks"
  xss@(x:xs) <- hostChunks
  if null xs
    then case () of
          _ | x == "localhost" -> pure Localhost
            | otherwise -> fail ("Only one term parsed: " ++ show xss)
    else let xss' :: Vector Text
             xss' = V.fromList xss
             unsnoc :: Vector a -> (Vector a, a)
             unsnoc x' =
               let (fs,l) = V.splitAt (V.length x' - 1) x'
               in  (fs, l V.! 0)
             (ns,c) = unsnoc xss'
         in  pure (N $ NormalHost ns c)
