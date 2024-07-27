{-# LANGUAGE QuasiQuotes #-}

module Common.Url where

import Config.Config (NetworkId (..), appNetwork)

import Data.Attoparsec.Text
    ( Parser
    , char
    , choice
    , parseOnly
    , sepBy1
    , takeWhile1
    , (<?>)
    )
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.RawString.QQ (r)
import Text.Regex.TDFA
    ( CompOption (lastStarGreedy)
    , ExecOption (captureGroups)
    , Regex
    , defaultCompOpt
    , defaultExecOpt
    , matchTest
    )
import Text.Regex.TDFA.Text (compile)

checkUrl :: Text -> Bool
checkUrl = regexPosixOpt urlRegexPosixPattern

regexPosixOpt :: Text -> Text -> Bool
regexPosixOpt rPattern input = matchTest rOpt input
    where
        rOpt :: Regex
        rOpt = case reg of
            Left err -> error err
            Right regex -> regex
        reg =
            compile
                defaultCompOpt{lastStarGreedy = True}
                defaultExecOpt{captureGroups = False}
                rPattern

urlRegexPosixPattern :: Text
urlRegexPosixPattern =
    [r|^https?://((25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)\.(25[0-5]|2[0-4][[:digit:]]|[01]?[[:digit:]][[:digit:]]?)|(([[:alnum:]]+|([[:alnum:]]+\-[[:alnum:]]*)*[[:alnum:]])(\.([[:alnum:]]+|([[:alnum:]]+\-[[:alnum:]]*)*[[:alnum:]]))*\.([[:alpha:]]{2,})))/$|]


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
    { uriHostName :: !(Vector Text)
    , urihHostSuffix :: !Text
    }

printHost :: Host -> Text
printHost x = case x of
    Localhost -> "localhost"
    N (NormalHost ns c) -> T.intercalate "." (V.toList (ns `V.snoc` c))

parseHost :: Parser Host
parseHost = do
    let hostChunk = takeWhile1 (\c -> c `notElem` ['.', ':', '/', '?', '#']) <?> "host chunk"
        hostChunks = hostChunk `sepBy1` char '.' <?> "host chunks"
    xss@(x : xs) <- hostChunks
    if null xs
        then case () of
            _
                | x == "localhost" -> pure Localhost
                | otherwise -> fail ("Only one term parsed: " ++ show xss)
        else
            let xss' :: Vector Text
                xss' = V.fromList xss
                unsnoc :: Vector a -> (Vector a, a)
                unsnoc x' =
                    let (fs, l) = V.splitAt (V.length x' - 1) x'
                     in (fs, l V.! 0)
                (ns, c) = unsnoc xss'
             in pure (N $ NormalHost ns c)

normalizePingUrl :: Text -> Text
normalizePingUrl url = T.append (T.dropWhileEnd (== '/') url) $ case appNetwork of
    Mainnet ->
        if T.isInfixOf "execute-api.eu-central-1.amazonaws.com" url
            then "//"
            else "/"
    Testnet -> "/"

normalizeCurrentUrl :: Text -> Text
normalizeCurrentUrl url = case url of
    "localhost:3000" -> "http://localhost:3000"
    u -> u