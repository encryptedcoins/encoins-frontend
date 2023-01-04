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

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)

--------------------------------------------- ByteString ----------------------------------------------

-- TODO: Some refactoring is needed. This code repeats our existing backend code!

emptyByteString :: BS.ByteString
emptyByteString = BS.empty

indexByteString :: BS.ByteString -> Integer -> Integer
indexByteString bs i = toInteger $ BS.index bs (fromInteger i)

sliceByteString :: Integer -> Integer -> BS.ByteString -> BS.ByteString
sliceByteString start n bs = BS.take (fromIntegral n) (BS.drop (fromIntegral start) bs)

dropByteString :: Integer -> BS.ByteString -> BS.ByteString
dropByteString n bs = sliceByteString n (toInteger (BS.length bs) - n) bs

byteStringToList :: BS.ByteString -> [Integer]
byteStringToList bs = indexByteString bs 0 : byteStringToList (dropByteString 1 bs)

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger bs = foldr (\d n -> 256*n + d) 0 (byteStringToList bs)

tryDecode :: Text -> Either String BS.ByteString
tryDecode = BS.decode . encodeUtf8