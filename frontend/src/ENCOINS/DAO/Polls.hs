module ENCOINS.DAO.Polls where

import           Data.Text                    (Text)
import           Reflex.Dom

import           ENCOINS.Common.Widgets.Basic (lnkInline, br)

data Poll m = Poll {
        pollNumber :: Integer,
        pollQuestion :: m (),
        pollSummary :: m (),
        pollAnswers :: [(Text, Text)],
        pollEnds :: Text
    }

poll5 :: MonadWidget t m => Poll m
poll5 = Poll 5
    (text "Poll #5: Do you approve the Treasury Allocation and Sustainable Development Plan proposed by the ENCOINS team?")
    (do
    text
        "The complete Treasury Allocation Plan was proposed recently by the ENCOINS team on "
    lnkInline "https://discord.com/channels/930855339501445151/1144309678352900134/1144309678352900134" "Discord"
    text ". The plan sets specific amounts for spending on audits, bug bounty program, liquidity provision, and team incentives. \
        \ It also contains updates to the protocol economics which we call the Sustained Development Plan. Check out the full proposal text "
    lnkInline "https://discord.com/channels/930855339501445151/1144309678352900134/1144309678352900134" "here"
    text "."
    )
    [("Yes", "100%")
    , ("No", "0%")]
    "4 Septempber 2023, 22:00 UTC"

poll4 :: MonadWidget t m => Poll m
poll4 = Poll 4
    (text "Poll #4: Spend up to 100k ENCS from the treasury on the VyFi partnership?")
    (do
        el "strong" $ text "Encoins partnership with VyFi"
        br
        br
        text "Total allocation for the program: 100,000 ENCS."
        br
        br
        el "strong" $ text "Farming Incentives"
        br
        text "Allocation: 70,000 ENCS."
        br
        text "The amount of ENCS starts at low quantities and increase as liquidity increases. With 70,000 ENCS we will have enough \
            \ tokens for more than 1 year of farming rewards, bootstrapping a high liquidity."
        br
        br
        el "strong" $ text "Stake ENCS, earn ENCS + VYFI"
        br
        text "Allocation: 20,000 ENCS."
        br
        text "This program aims to compensate our holders. It will end when the available tokens are distributed, or when the protocol \
            \ is launched on mainnet, whichever comes first. In addition, the VYFI team also gives us a certain number of tokens to \
            \ distribute among all those who participate in this program."
        br
        br
        el "strong" $ text "Stake VYFI, earn ENCS"
        br
        text "Allocation: 10,000 ENCS."
        br
        text "This program aims to unite both communities, trying to attract users from VYFI to Encoins. Just as they will give us \
            \ VYFI for the aforementioned program, it is fair that we give something to their community..."
        br
        br
        text "For more information on the proposal, visit our "
        lnkInline "https://discord.com/channels/930855339501445151/1136310556026994748" "Discord server"
        text "."
    )
    [("Yes", "91.55%")
    , ("No", "8.45%")]
    "13 August 2023, 22:00 UTC"

poll3 :: MonadWidget t m => Poll m
poll3 = Poll 3
    (text "Poll #3: Spend 250k ENCS from the treasury on the ENCOINS v1 protocol audit?")
    (do
    text
        "The protocol audit will be performed by Anastasia Labs. The 250k ENCS will be put into the two-year linear vesting \
        \ contract with monthly payments of 10415 ENCS. For more information on the proposal, visit our "
    lnkInline "https://discord.com/channels/930855339501445151/1120801429112762448" "Discord server"
    text "."
    )
    [("Yes", "97.6%")
    , ("No", "2.4%")]
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