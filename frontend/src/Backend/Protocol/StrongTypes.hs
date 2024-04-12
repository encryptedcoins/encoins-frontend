module Backend.Protocol.StrongTypes
  (
    PasswordHash (getPassHash)
  , toPasswordHash
  ) where

import           Backend.Utility (hashKeccak512)

import           Data.Text       (Text)
import qualified Data.Text       as T

-- AVOID exporting constructor 'PasswordHash'
-- USE smart constructor 'toPasswordHash'
-- TODO: consider using refined types from refined library
newtype PasswordHash = PasswordHash { getPassHash :: Text } deriving (Eq, Show)

toPasswordHash :: Text -> Maybe PasswordHash
toPasswordHash p
  | p == T.empty = Nothing
  | p == hashKeccak512 "" = Nothing
  | otherwise = Just $ PasswordHash p
