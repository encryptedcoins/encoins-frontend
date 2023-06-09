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
    "Poll #1: Do you support the proposal to use 50k ENCS from the treasury to provide liquidity to the ENCS/ADA pool on MinSwap?"
    "The ENCS/ADA liquidity pool on MinSwap has about 120k ENCS in it (as of June 9, 2023). \
    \ Adding additional 50k ENCS from the treasury should improve the price stability of ENCS. \
    \ The total treasury holdings are currently at 3m ENCS. In case the decision is later reversed, \
    \ the corresponding combination of ADA and ENCS will be returned to the treasury."
    ["Yes", "No"]
    "13 June 2023, 22:00 UTC"