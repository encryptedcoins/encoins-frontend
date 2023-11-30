{-# LANGUAGE QuasiQuotes #-}

module ENCOINS.Common.Utils where

import           Control.Monad        (guard)
import           Data.Text            (Text, pack)
import qualified Data.Text            as T
import           Text.RawString.QQ (r)
import           Text.Regex.TDFA      (CompOption (lastStarGreedy),
                                       ExecOption (captureGroups), Regex,
                                       defaultCompOpt, defaultExecOpt,
                                       matchTest)
import           Text.Regex.TDFA.Text (compile)

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
      Left err -> error err
      Right regex  -> regex
    reg = compile
      defaultCompOpt{lastStarGreedy=True}
      defaultExecOpt{captureGroups=False}
      rPattern

urlRegexPosixPattern :: Text
urlRegexPosixPattern = [r|^https?://((([1-9][[:digit:]]?|1[[:digit:]][[:digit:]]|2[01][[:digit:]]|22[0-3])(\.(1?[[:digit:]]{1,2}|2[0-4][[:digit:]]|25[0-5])){2}(\.([1-9][[:digit:]]?|1[[:digit:]][[:digit:]]|2[0-4]|25[0-4])))|((([[:alnum:]]-[[:alnum:]]{0,62})?[[:alnum:]]+\.)+([[:alpha:]]{2,})))(:[[:digit:]]{2,5})?(/[0-9a-zA-Z$-_.+!*'(),%?&=:@/;~#]*)?$|]
