module Backend.Utility where

import           Config.Config          (NetworkId (..), appNetwork)

import           Control.Monad          (join, (<=<))
import           Control.Monad.IO.Class (liftIO)
import qualified CSL
import           Data.Functor           ((<&>))
import qualified Data.Map               as Map
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.UUID              as Uid
import qualified Data.UUID.V4           as Uid
import           Reflex.Dom
import           Witherable             (catMaybes)

normalizePingUrl :: Text -> Text
normalizePingUrl url = T.append (T.dropWhileEnd (== '/') url) $ case appNetwork of
  Mainnet -> if T.isInfixOf "execute-api.eu-central-1.amazonaws.com" url
    then "//"
    else "/"
  Testnet -> "/"

normalizeCurrentUrl :: Text -> Text
normalizeCurrentUrl url = case url of
  "localhost:3000" -> "http://localhost:3000"
  u                -> u

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a)  = Right a

switchHoldDyn :: MonadWidget t m
  => Dynamic t a
  -> (a -> m (Event t b))
  -> m (Event t b)
switchHoldDyn da f = switchHold never <=< dyn $ da <&> f

dynHoldDyn :: MonadWidget t m
  => Dynamic t a
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
        Just i  -> maybe False (const True) $ Map.lookup token i

eventMaybe :: Reflex t => e -> Event t (Maybe a) -> (Event t e, Event t a)
eventMaybe errValue ev = (errValue <$ ffilter isNothing ev, catMaybes ev)

eventMaybeDynDef :: Reflex t
  => Dynamic t def
  -> Event t (Maybe a)
  -> (Event t def, Event t a)
eventMaybeDynDef dDefault ev = (tagPromptlyDyn dDefault $ ffilter isNothing ev, catMaybes ev)

eventEither :: Reflex t => Event t (Either e a) -> (Event t e, Event t a)
eventEither ev = (filterLeft ev, filterRight ev)

eventTuple :: Reflex t => Event t (a,b) -> (Event t a, Event t b)
eventTuple ev  = (fst <$> ev, snd <$> ev)

space :: Text
space = " "

column :: Text
column = ":"

toText :: Show a => a -> Text
toText = T.pack . show

-- resId should be unique in every call of elementResultJS
-- genUid generates unique resId.
genUid :: MonadWidget t m => Event t () -> m (Event t Text)
genUid ev = performEvent $ (Uid.toText <$> liftIO Uid.nextRandom) <$ ev
