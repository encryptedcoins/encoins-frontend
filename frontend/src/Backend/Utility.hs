module Backend.Utility where

import           Data.Text (Text)
import qualified Data.Text as T


normalizePingUrl :: Text -> Text
normalizePingUrl t = T.append (T.dropWhileEnd (== '/') t) "//"
