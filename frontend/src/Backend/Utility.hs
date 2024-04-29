module Backend.Utility where

import Config.Config (NetworkId (..), appNetwork)

import qualified CSL
import Control.Monad (join, (<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.Keccak as Keccak
import Data.ByteString.Base16 as BS16
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.UUID as Uid
import qualified Data.UUID.V4 as Uid
import Reflex.Dom
import Witherable (catMaybes)

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

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a) = Right a

switchHoldDyn ::
    (MonadWidget t m) =>
    Dynamic t a
    -> (a -> m (Event t b))
    -> m (Event t b)
switchHoldDyn da f = switchHold never <=< dyn $ da <&> f

dynHoldDyn ::
    (MonadWidget t m) =>
    Dynamic t a
    -> b
    -> (a -> m (Dynamic t b))
    -> m (Dynamic t b)
dynHoldDyn dA b f = do
    edB <- dyn $ f <$> dA
    join <$> holdDyn (constDyn b) edB

isMultiAssetOf :: Text -> Text -> CSL.MultiAsset -> Bool
isMultiAssetOf symbol token (CSL.MultiAsset mp) =
    case Map.lookup symbol mp of
        Nothing -> False
        Just i -> maybe False (const True) $ Map.lookup token i

eventMaybe :: (Reflex t) => Event t (Maybe a) -> (Event t (), Event t a)
eventMaybe ev = (() <$ ffilter isNothing ev, catMaybes ev)

eventMaybeDynDef ::
    (Reflex t) =>
    Dynamic t def
    -> Event t (Maybe a)
    -> (Event t def, Event t a)
eventMaybeDynDef dDefault ev = (tagPromptlyDyn dDefault $ ffilter isNothing ev, catMaybes ev)

eventTuple :: (Reflex t) => Event t (a, b) -> (Event t a, Event t b)
eventTuple ev = (fst <$> ev, snd <$> ev)

dynTuple ::
    (MonadWidget t m) =>
    a
    -> b
    -> Event t (a, b)
    -> m (Dynamic t a, Dynamic t b)
dynTuple aDef bDef eAB = do
    da <- holdDyn aDef $ fst <$> eAB
    db <- holdDyn bDef $ snd <$> eAB
    pure (da, db)

space :: Text
space = " "

column :: Text
column = ":"

toText :: (Show a) => a -> Text
toText = T.pack . show

-- resId should be unique in every call of elementResultJS
-- genUid generates unique resId.
genUid :: (MonadWidget t m) => Event t () -> m (Event t Text)
genUid ev = performEvent $ (Uid.toText <$> liftIO Uid.nextRandom) <$ ev

-- Keccak 512 is SHA-3. It is used in cryptoJS
-- we use it to replace js external library
hashKeccak512 :: Text -> Text
hashKeccak512 raw =
    TE.decodeUtf8 $
        BS16.encode $
            Keccak.keccak512 $
                TE.encodeUtf8 raw

isHashOfRaw :: Text -> Text -> Bool
isHashOfRaw hash raw = hash == hashKeccak512 raw

formatPollTime :: UTCTime -> Text
formatPollTime = T.pack . formatTime defaultTimeLocale "%e %B %Y, %R %Z"

formatCoinTime :: UTCTime -> Text
formatCoinTime = T.pack . formatTime defaultTimeLocale "%Y-%B-%e"
