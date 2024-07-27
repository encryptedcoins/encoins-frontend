module Common.Utility where

import Control.Monad (guard)
import qualified Crypto.Hash.Keccak as Keccak
import qualified Data.Aeson as A (ToJSON, encode)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)



-------------------------------------------------------------------------------
-- List stuff
-------------------------------------------------------------------------------

-- O(n * log(n)) instead of O(n^2) in 'nubBy'
-- It reverse list of tokens
nubWith :: (Ord b) => (a -> b) -> [a] -> [a]
nubWith f l = snd $ foldl' (func f) (Set.empty, []) l
    where
        func f' (set, acc) x
            | Set.member x' set = (set, acc)
            | otherwise = (Set.insert x' set, x : acc)
            where
                x' = f' x

-- Combine two lists efficiently excluding duplicates from both lists
-- Original 'union' excludes duplicates from last list only.
-- O((n+m) * log(n+m)) instead of > O(n+m^2)
-- Give the priority to first list.
-- It reverse list of tokens
unionWith :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
unionWith f l1 l2 =
    let func f' (set, acc) x
            | Set.member x' set = (set, acc)
            | otherwise = (Set.insert x' set, x : acc)
            where
                x' = f' x
        listOfUniq = snd $ foldl' (func f) (Set.empty, []) $ l1 <> l2
     in listOfUniq

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
    where
        go [] _ = Nothing
        go (x : _) 0 = Just x
        go (_ : xs) i = go xs (pred i)

-- it added to base from 4.15.0.0
-- we are on base-4.12.0.0
singletonL :: a -> [a]
singletonL x = [x]

-------------------------------------------------------------------------------
-- Text stuff
-------------------------------------------------------------------------------

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a) = Right a

textMaybe :: Text -> Maybe Text
textMaybe txt = bool (Just txt) Nothing $ T.null txt

space :: Text
space = " "

column :: Text
column = ":"

toText :: (Show a) => a -> Text
toText = T.pack . show

-------------------------------------------------------------------------------
-- Hash stuff
-------------------------------------------------------------------------------

data HashBit = B512 | B384 | B256 | B224
    deriving (Eq)

hashKeccak :: HashBit -> Text -> Text
hashKeccak hb raw =
    let keccak = case hb of
            B512 -> Keccak.keccak512
            B384 -> Keccak.keccak384
            B256 -> Keccak.keccak256
            B224 -> Keccak.keccak224
     in TE.decodeUtf8 $ BS16.encode $ keccak $ TE.encodeUtf8 raw

hashKeccak512 :: Text -> Text
hashKeccak512 = hashKeccak B512

hashKeccak256 :: Text -> Text
hashKeccak256 = hashKeccak B256

isHashOfRaw :: Text -> Text -> Bool
isHashOfRaw hash raw = hash == hashKeccak512 raw

-------------------------------------------------------------------------------
-- Time stuff
-------------------------------------------------------------------------------

formatPollTime :: UTCTime -> Text
formatPollTime = T.pack . formatTime defaultTimeLocale "%e %B %Y, %R %Z"

formatCoinTime :: UTCTime -> Text
formatCoinTime = T.pack . formatTime defaultTimeLocale "%Y-%B-%e"

-------------------------------------------------------------------------------
-- JSON stuff
-------------------------------------------------------------------------------

toJsonText :: (A.ToJSON a) => a -> Text
toJsonText = TE.decodeUtf8 . toJsonStrict

toJsonStrict :: (A.ToJSON a) => a -> ByteString
toJsonStrict = toStrict . A.encode
