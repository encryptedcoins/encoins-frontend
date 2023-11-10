module ENCOINS.Website.Widgets.ISPOPage (ispoPage) where

import           Control.Monad                 (void)
import           Reflex.Dom

import           ENCOINS.App.Widgets.ISPO      (calculator)
import           ENCOINS.Common.Widgets.Basic
import           ENCOINS.Website.Widgets.Basic

ispoPage :: MonadWidget t m => m ()
ispoPage = do
    -- Printing ISPO page
    titleSection
    aboutSection
    rewardsCalculatorSection
    lotteryWinnersSection

titleSection :: MonadWidget t m => m ()
titleSection = section "" "" $ do
    container "" $ divClass "div-vertical-space-56" blank
    container "" $ h1Mini "INITIAL STAKE POOL OFFERING"
    container "" $ h2 "Participate in the ISPO to support our project!"
    container "" $ divClass "div-vertical-space-78" blank
    blank

aboutSection :: MonadWidget t m => m ()
aboutSection = section "" "" $ do
    container "" $ h3 "ABOUT ISPO"
    container "" $ do
        pClass "p-ispo" $ do
            el "strong" $ text "Initial stake pool offering (ISPO)"
            text " is a novel method to distribute a protocol's utility tokens to the community using the Cardano blockchain. It ensures a high degree of decentralization of the token ownership and simultaneously supports the project's development."
        pClass "p-ispo" $ do
            text "Currently, we are distributing a total of 10 000 000 ENCS tokens (from the total supply of 15 000 000 ENCS) to delegators of our very own stake pool with the ticker symbol "
            el "strong" $ text "[CMIX]"
            text ". Our ISPO, initially started on Epoch 319 (early February 2022) as CardMix ISPO, will last until all tokens are assigned. To participate in the token distribution, users delegate their ADA to our stake pool "
            el "strong" $ text "[CMIX]"
            text ". The ENCS rewards are calculated for every epoch individually, so users may decide to participate for any number of epochs. Our pool is also a part of "
            el "strong" $ text "Cardano Single Pool Alliance (CSPA)"
            text "."
        pClass "p-ispo" $ do
            text "Our pool: "
            el "strong" $ do
                text "pool1pmeetaqhlsdc5yj8snp8j46hprs9mzghxcl63vch95trz3xwpln"
                el "br" blank
            text "In hex format: "
            el "strong" $ text "0ef395f417fc1b8a124784c279575708e05d8917363fa8b3172d1631"
        pClass "p-ispo" $ do
            text "Check the pool on third-party aggregators: "
            lnkInline "https://cexplorer.io/pool/pool1pmeetaqhlsdc5yj8snp8j46hprs9mzghxcl63vch95trz3xwpln" "cexplorer.io"
            text ", "
            lnkInline "https://adapools.org/pool/0ef395f417fc1b8a124784c279575708e05d8917363fa8b3172d1631" "adapools.org"
            text ", "
            lnkInline "https://pooltool.io/pool/0ef395f417fc1b8a124784c279575708e05d8917363fa8b3172d1631/epochs" "pooltool.io"
            text "."
            el "br" blank
            text "You can delegate your ADA to a stake pool in your Cardano wallet app. It is completely safe, as your funds never leave your wallet. Enter CMIX in the pool search textbox inside your wallet app to find our stake pool. To be sure you are delegating to the right pool, check the pool ID."

rewardsCalculatorSection :: MonadWidget t m => m ()
rewardsCalculatorSection = section "" "" $ do
    container "" $ h3 "REWARDS CALCULATOR"
    container "" $ do
        pClass "p-ispo" $ text "Participants of the ISPO can track their accumulated rewards using the calculator below. Enter your stake key to see how many ENCS tokens you have secured so far. The information is updated every epoch (5 days). Type your stake address below:"
        calculator
        pClass "p-ispo" $ do
            el "strong" $ text "Distributed in the ISPO:"
            text " 9 899 755 ENCS (out of 10 000 000 ENCS)."
        pClass "p-ispo" $ do
            text "Each epoch, we distribute between 120 000 and 600 000 tokens depending on the pool saturation. The per-ADA rewards are higher when the saturation is low. 90% of ENCS tokens are distributed proportionally to your delegation. The rest is distributed through a lottery. Every ISPO participant with at least 1000 ADA stake is eligible. You can check the rest of the details about the ISPO "
            lnkInline "https://encoins-crypto.medium.com/all-ispo-info-summarized-c68e33747b77" "here"
            text "."

lotteryWinnersSection :: MonadWidget t m => m ()
lotteryWinnersSection = section "" "" $ do
    container "" $ h3 "EPOCH 396 LOTTERY WINNERS"
    container "" $ do
        pClass "p-ispo" $ do
            text "At the start of each epoch, we use the hashes of the first few blocks to determine our winners. Simultaneously, we publish the list of participants for the next lottery. You can find it "
            lnkInline "https://encoins.io/ispo/396.txt" "here"
            text ". Below are the last epoch's winners."
        divClass "div-lottery-winners" $ do
            void $ image "fireworks.svg" "image-fireworks inverted" "120px"
            elAttr "ul" ("role" =: "list" <> "class" =: "list p-ispo") $ do
                mapM_ (el "li" . text)
                    [
                        "stake1ux3475zfrwg4dvnyungrs8d63q6h98fm7ckettl2q9keq4gng2h40",
                        "stake1uyvuffml95dt5a9edvyf2caf82qsjw6q7es9r856w7e88cgzgrs53",
                        "stake1u8jzqk9h5sntckefv49c4qnmu0rf0c9gwnklsetgvchsgtsqs97ph"
                    ]
            void $ image "fireworks.svg" "image-fireworks inverted" "120px"
        pClass "p-ispo" $ text "The winners secure additional 4000 ENCS. Congratulations!!!"
