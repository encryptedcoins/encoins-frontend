module ENCOINS.DAO.Polls where

import           Data.IntMap.Strict           (IntMap, fromList, mapEither)
import           Data.Text                    (Text, pack)
import           Data.Time                    (LocalTime (LocalTime), UTCTime,
                                               fromGregorianValid,
                                               localTimeToUTC,
                                               makeTimeOfDayValid, utc)
import           Reflex.Dom

import           Backend.Utility              (column, formatPollTime, space,
                                               toText)
import           ENCOINS.Common.Widgets.Basic (br, lnkInline)
import           ENCOINS.DAO.PollResults


data Poll m = Poll
  { pollNumber      :: Int
  , pollQuestion    :: m ()
  , pollSummary     :: m ()
  , pollAnswers     :: VoteResult
  , pollFullResults :: Text
  , pollEnds        :: UTCTime
  }

polls :: MonadWidget t m => IntMap (Poll m)
polls = fromList $ zip [1..]
  [
    poll1 1 result1 resultFull1
  , poll2 2 result2 resultFull2
  , poll3 3 result3 resultFull3
  , poll4 4 result4 resultFull4
  , poll5 5 result5 resultFull5
  , poll6 6 result6 resultFull6
  ]

poll6 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll6 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Do you approve performing a Collective Zap-In from the treasury for a total of 150k $ADA, including rewards?")
    (do
    text
        "In the first instance, allocating 600k $ENCS to hold a CZI on Minswap generated a lot of controversy in our community. \
        \ An idea that was well accepted was to do the CZI with Minswap but with a smaller allocation: 150k $ENCS."
    br
    br
    text
        "With the current price of 1 $ENCS = 1.73 $ADA our Minswap pool is approximately 280k $ADA + 163k ENCS. We propose to \
        \allocate 120k $ENCS (more than 70% of the current liquidity) to make a smaller CZI, and 30k $ENCS as special rewards \
        \for an exclusive pool only for those CZI participants, where they will be able to get higher rewards than the original pool."
    br
    br
    text
        "It is important to note that these funds being requested to the DAO will continue to be held by the DAO in the form of $ENCS/$ADA LP tokens."
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 11 17)

poll5 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll5 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Do you approve the Treasury Allocation and Sustainable Development Plan proposed by the ENCOINS team?")
    (do
    text
        "The complete Treasury Allocation Plan was proposed recently by the ENCOINS team on "
    lnkInline "https://discord.com/channels/930855339501445151/1144309678352900134/1144309678352900134" "Discord"
    text ". The plan sets specific amounts for spending on audits, bug bounty program, liquidity provision, and team incentives. \
        \ It also contains updates to the protocol economics which we call the Sustained Development Plan. Check out the full proposal text "
    lnkInline "https://discord.com/channels/930855339501445151/1144309678352900134/1144309678352900134" "here"
    text "."
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 9 4)


poll4 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll4 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Spend up to 100k $ENCS from the treasury on the VyFi partnership?")
    (do
        el "strong" $ text "Encoins partnership with VyFi"
        br
        br
        text "Total allocation for the program: 100,000 $ENCS."
        br
        br
        el "strong" $ text "Farming Incentives"
        br
        text "Allocation: 70,000 $ENCS."
        br
        text "The amount of $ENCS starts at low quantities and increase as liquidity increases. With 70,000 $ENCS we will have enough \
            \ tokens for more than 1 year of farming rewards, bootstrapping a high liquidity."
        br
        br
        el "strong" $ text "Stake $ENCS, earn $ENCS + VYFI"
        br
        text "Allocation: 20,000 $ENCS."
        br
        text "This program aims to compensate our holders. It will end when the available tokens are distributed, or when the protocol \
            \ is launched on mainnet, whichever comes first. In addition, the VYFI team also gives us a certain number of tokens to \
            \ distribute among all those who participate in this program."
        br
        br
        el "strong" $ text "Stake $VYFI, earn $ENCS"
        br
        text "Allocation: 10,000 $ENCS."
        br
        text "This program aims to unite both communities, trying to attract users from VYFI to Encoins. Just as they will give us \
            \ VYFI for the aforementioned program, it is fair that we give something to their community..."
        br
        br
        text "For more information on the proposal, visit our "
        lnkInline "https://discord.com/channels/930855339501445151/1136310556026994748" "Discord server"
        text "."
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 8 13)


poll3 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll3 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Spend 250k $ENCS from the treasury on the ENCOINS v1 protocol audit?")
    (do
    text
        "The protocol audit will be performed by Anastasia Labs. The 250k $ENCS will be put into the two-year linear vesting \
        \ contract with monthly payments of 10415 $ENCS. For more information on the proposal, visit our "
    lnkInline "https://discord.com/channels/930855339501445151/1120801429112762448" "Discord server"
    text "."
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 7 7)


poll2 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll2 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Spend 50k $ENCS from the treasury as rewards for incentivized farming on MinSwap?")
    (do
    text
        "This proposal is to set up an incentivized farm for the $ENCS/$ADA liquidity pool on MinSwap. \
        \ Once all MinSwap's conditions are satisfied, the farm can be activated (read MinSwap docs "
    lnkInline "https://docs.minswap.org/faq/token-launching-and-farming/4.-farming-for-projects" "here"
    text
        "). \
        \ Liquidity providers of the $ENCS/$ADA pool will receive both $ENCS and MIN as extra rewards. \
        \ The aim of this proposal is to boost $ENCS liquidity by increasing incentives to the liquidity providers. "
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 6 25)


poll1 :: MonadWidget t m => Int -> VoteResult -> Text -> Poll m
poll1 n voteRes voteFullRes = Poll n
    (text $ pollNum n <> "Do you support the proposal to use 50k $ENCS from the treasury to provide liquidity to the $ENCS/$ADA pool on MinSwap?")
    (text
        "The $ENCS/$ADA liquidity pool on MinSwap has about 120k $ENCS in it (as of June 9, 2023). \
        \ Adding additional 50k $ENCS from the treasury should improve the price stability of $ENCS. \
        \ The total treasury holdings are currently at 3m $ENCS. In case the decision is later reversed, \
        \ the corresponding combination of $ADA and $ENCS will be returned to the treasury."
    )
    voteRes
    voteFullRes
    (endTime22x00 2023 6 13)

-- Help functions for polls

pollNum :: Int -> Text
pollNum n = "Poll #" <> toText n <> column <> space

endTime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
endTime year month day hour minute = localTimeToUTC utc localTime
  where
    localTime = LocalTime day' time
    time = case makeTimeOfDayValid hour minute 0 of
      Nothing -> error "Invalid parameters of makeTimeOfDayValid"
      Just t  -> t
    day' = case fromGregorianValid year month day of
      Nothing -> error "Invalid parameters of fromGregorianValid"
      Just d' -> d'

endTime22x00 :: Integer -> Int -> Int -> UTCTime
endTime22x00 year month day = endTime year month day 22 0

poolsActiveAndArchived :: MonadWidget t m
  => UTCTime
  -> (IntMap (Poll m), IntMap (Poll m))
poolsActiveAndArchived utcTime = mapEither (dividePolls utcTime) polls

dividePolls :: MonadWidget t m
  => UTCTime
  -> Poll m
  -> Either (Poll m) (Poll m)
dividePolls nowTime poll
  | pollEnds poll > nowTime = Right poll
  | otherwise = Left poll
