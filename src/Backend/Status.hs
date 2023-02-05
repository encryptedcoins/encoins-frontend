module Backend.Status where

import           Data.Text                     (Text)

data Status = Ready | Balancing | Signing | Submitting | Submitted | StatusError Text
    deriving (Eq)

instance Show Status where
    show Ready           = "The transaction has been confirmed."
    show Balancing       = "Balancing..."
    show Signing         = "Please sign the transaction."
    show Submitting      = "Submitting..."
    show Submitted       = "The transaction is now pending..."
    show (StatusError e) = "Error: " <> show e