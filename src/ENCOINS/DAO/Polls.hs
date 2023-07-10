module ENCOINS.DAO.Polls where

import           Data.Text                    (Text)
import           Reflex.Dom

import           ENCOINS.Common.Widgets.Basic (lnkInline)

data Poll m = Poll {
        pollNumber :: Integer,
        pollQuestion :: m (),
        pollSummary :: m (),
        pollAnswers :: [(Text, Text)],
        pollEnds :: Text
    }

poll3 :: MonadWidget t m => Poll m
poll3 = Poll 3
    (text "Poll #3: Spend 250k ENCS from the treasury on the ENCOINS v1 protocol audit?")
    (do
    text
        "The protocol audit will be performed by Anastasia Labs. The 250k ENCS will be put into the two-year linear vesting \
        \ contract with monthly payments of 10415 ENCS. For more information on the proposal, visit our "
    lnkInline "https://discord.com/channels/930855339501445151/1120801429112762448/1120801429112762448" "Discord server"
    text "."
    )
    [("Yes", "84.9%")
    , ("No", "15.1%")]
    "7 July 2023, 22:00 UTC"

poll2 :: MonadWidget t m => Poll m
poll2 = Poll 2
    (text "Poll #2: Spend 50k ENCS from the treasury as rewards for incentivized farming on MinSwap?")
    (do
    text
        "This proposal is to set up an incentivized farm for the ENCS/ADA liquidity pool on MinSwap. \
        \ Once all MinSwap's conditions are satisfied, the farm can be activated (read MinSwap docs "
    lnkInline "https://docs.minswap.org/faq/token-launching-and-farming/4.-farming-for-projects" "here"
    text 
        "). \
        \ Liquidity providers of the ENCS/ADA pool will receive both ENCS and MIN as extra rewards. \
        \ The aim of this proposal is to boost ENCS liquidity by increasing incentives to the liquidity providers. "
    )
    [("Yes", "97.97%")
    , ("No", "2.03%")]
    "25 June 2023, 22:00 UTC"

poll1 :: MonadWidget t m => Poll m
poll1 = Poll 1
    (text "Poll #1: Do you support the proposal to use 50k ENCS from the treasury to provide liquidity to the ENCS/ADA pool on MinSwap?")
    (text
        "The ENCS/ADA liquidity pool on MinSwap has about 120k ENCS in it (as of June 9, 2023). \
        \ Adding additional 50k ENCS from the treasury should improve the price stability of ENCS. \
        \ The total treasury holdings are currently at 3m ENCS. In case the decision is later reversed, \
        \ the corresponding combination of ADA and ENCS will be returned to the treasury."
    )
    [("Yes", "95.9%"), ("No", "4.1%")]
    "13 June 2023, 22:00 UTC"