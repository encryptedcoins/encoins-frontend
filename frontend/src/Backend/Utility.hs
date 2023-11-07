module Backend.Utility where

import           Data.Text (Text)
import qualified Data.Text as T


normalizePingUrl :: Text -> Text
normalizePingUrl t = T.append (T.dropWhileEnd (== '/') t) "//"

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a) = Right a