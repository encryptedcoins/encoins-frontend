module ENCOINS.Website.Widgets.LandingPage (
    titleSectionWidget,
    communitySectionWidget,
    featuresSectionWidget,
    roadmapSectionWidget
    ) where

import           Control.Monad          (when)
import           Data.Bool              (bool)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Reflex.Dom
import ENCOINS.Website.Widgets.Basic

ourResourses :: MonadWidget t m => Text -> m ()
ourResourses w = do
    resourseButton "" "https://twitter.com/ENCOINS1" "Twitter.svg" w
    resourseButton "" "https://discord.gg/Q3gPP87Tcw" "Discord.svg" w
    resourseButton "" "https://encoins-crypto.medium.com/" "Medium.svg" w
    resourseButton "" "mailto:team@encoins.io" "Email.svg" w
    resourseButton "" "https://t.me/encoins_io" "Telegram.svg" w
    resourseButton "image-disabled" "#" "GitBook.svg" w
    resourseButton "image-disabled" "#" "GitHub.svg" w
    resourseButton "" "https://www.youtube.com/channel/UCk4QtReP4kQKfIIoWw7Q1wg" "YouTube.svg" w

titleSectionWidget :: MonadWidget t m => m ()
titleSectionWidget = section "" $ do
    container "" $ h4 "Protect your privacy with"
    container "" $ h1 "ENCOINS"
    container "" $ h2 "Cardano Native Tokens with Encrypted Redeeming Values"
    _ <- container "container-extra-margin-small" $ btn "button-disabled" "Launch App"
    blank

communitySectionWidget :: MonadWidget t m => m ()
communitySectionWidget = section "" $ container "" $ do
    h4 "Join our community"
    divClass "div-our-resourses" $ ourResourses "83"
    blank

featuresSectionWidget :: MonadWidget t m => m ()
featuresSectionWidget = section "Features" $ do
    container "" $ h3 "INTRODUCTION"
    explainer "How to mint?" "Send ADA into the protocol to mint a bundle of NFTs (aka ENCOINS). Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
    explainer "How to use?" "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded account system."
    explainer "How to redeem?" "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    _ <- container "container-extra-margin-small" $ btn "button-disabled" "Launch App"
    blank
    where
        explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
            h4 txtTitle
            pClass "p-explainer" $ text txtExplainer

roadmapSectionWidget :: MonadWidget t m => m ()
roadmapSectionWidget = section "Roadmap" $ do
    container "" $ h3 "ROADMAP"
    roadmapItemRight "01" "Token Generation Event" "Shortly after our ISPO ends in February 2023, we will have the final ENCS token distribution. ENCS can then be generated and distributed using the algorithm presented here."
    roadmapItemLeft  "02" "Public Test" "In March 2023, ENCOINS Public Test will commence. Users will be able to test the base dApp on Cardano testnets. In the second phase, we expect relayers to join in."
    roadmapItemRight "03" "Mainnet Launch" "After the Public Test, everything will be ready for the mainnet launch. Users can now mint, trade, and transfer ENCOINS using their wallet or ENCOINS Ledger."
    roadmapItemLeft  "04" "ENSHARE Integrations" "ENSHARE, our upcoming protocol for confidential data sharing, will be integrated with ENCOINS, enabling on-chain sharing of minting keys."
    roadmapItemRight "05" "Ecosystem Integrations" "We plan to integrate ENCOINS with selected Cardano Ecosystem projects, including wallets, DeFi, and NFT dApps. All potential partners are wellcome!"
    where
        roadmapItemRight num txtTitle txtDesc = divClass "div-roadmap-item div-roadmap-item-right" $ do
            divClass "text-roadmap-numbers" $ text num
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" $ text txtDesc
            image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
        roadmapItemLeft num txtTitle txtDesc  = divClass "div-roadmap-item div-roadmap-item-left" $ do
            image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" $ text txtDesc
            divClass "text-roadmap-numbers" $ text num
            
    