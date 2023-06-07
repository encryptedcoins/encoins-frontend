module ENCOINS.DAO.Polls where

import Data.Text (Text)

data Poll = Poll {
        pollNumber :: Integer,
        pollQuestion :: Text,
        pollSummary :: Text,
        pollAnswers :: [Text],
        pollEnds :: Text
    }
    deriving (Eq, Show)

poll1 :: Poll
poll1 = Poll 1
    "Provide 50k ENCS from the treasury to the ENCS/ADA pool on MinSwap?"
    "Currently, the ENCS/ADA liquidity pool on MinSwap has about 120k ENCS in it. Adding additional 50k ENCS from the treasury should improve the price stability of ENCS."
    ["Yes", "No"]
    "11 June 2023, 22:00 UTC"