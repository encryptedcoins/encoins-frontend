module Backend.Utility where

import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom
import           Control.Monad             ((<=<))
import           Data.Functor              ((<&>))

normalizePingUrl :: Text -> Text
normalizePingUrl t = T.append (T.dropWhileEnd (== '/') t) "//"

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a) = Right a

switchHoldDyn :: MonadWidget t m
  => Dynamic t a
  -> (a -> m (Event t b))
  -> m (Event t b)
switchHoldDyn da f = switchHold never <=< dyn $ da <&> f