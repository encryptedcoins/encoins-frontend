module Widgets.Utils where

import           Control.Monad            (guard)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as BS
import           Data.Maybe               (isNothing)
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Reflex.Dom
import           Witherable               (catMaybes)

toText :: Show a => a -> Text
toText = pack . show

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _ = Nothing
    go  (x:_) 0 = Just x
    go  (_:xs) i = go xs (pred i)