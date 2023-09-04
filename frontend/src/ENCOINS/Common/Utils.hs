module ENCOINS.Common.Utils where

import           Control.Monad            (guard)
import           Data.Text                (Text, pack)

toText :: Show a => a -> Text
toText = pack . show

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _ = Nothing
    go  (x:_) 0 = Just x
    go  (_:xs) i = go xs (pred i)