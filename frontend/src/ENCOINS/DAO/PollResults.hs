{-# LANGUAGE DeriveAnyClass #-}

module ENCOINS.DAO.PollResults where

import           Data.Aeson           (FromJSON (..), ToJSON (..), encode)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics         (Generic)

data VoteResult = VoteResult
  { number :: Int
  , yes    :: Double
  , no     :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

toJsonResult :: VoteResult -> ByteString
toJsonResult = BL.toStrict . encode

result1 :: VoteResult
result1 = VoteResult 1 95.88470922943088 4.1152907705691115

result2 :: VoteResult
result2 = VoteResult 2 97.96947251298073 2.030527487019273

result3 :: VoteResult
result3 = VoteResult 3 84.86176407974799 15.138235920252008

result4 :: VoteResult
result4 = VoteResult 4 91.7209266164092 8.2790733835908

result5 :: VoteResult
result5 = VoteResult 5 100.0 0.0

result6 :: VoteResult
result6 = VoteResult 6 78.90642611077254 21.09357388922746
