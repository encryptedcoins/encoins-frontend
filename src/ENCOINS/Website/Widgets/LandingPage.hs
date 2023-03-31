module ENCOINS.Website.Widgets.LandingPage (landingPage) where

import           Control.Monad                     (void)
import           Data.Bool                         (bool)
import           Data.Text                         (Text)
import           Reflex.Dom
import           Reflex.ScriptDependent            (widgetHoldUntilDefined)

import           ENCOINS.Common.Widgets.Basic
import           ENCOINS.Website.Widgets.Basic
import           ENCOINS.Website.Widgets.Resourses (ourResourses)
import           JS.Website                        (scrollIntoView)

landingPage :: MonadWidget t m => Text -> m (Event t (Text, Text))
landingPage elemId = do
    -- Scrolling to a specific element
    ePB <- getPostBuild
    eWebpageLoaded <- updated <$> widgetHoldUntilDefined "scrollIntoView" ("js/Webpage.js" <$ ePB) blank blank
    performEvent_ (scrollIntoView elemId <$ eWebpageLoaded)

    -- Printing landing page
    titleSection
    communitySection
    featuresSection
    roadmapSection
    partnersSection
    
    return never

titleSection :: MonadWidget t m => m ()
titleSection = section "" "" $ do
    container "" $ h4 "Protect your privacy with"
    container "" $ h1 "ENCOINS"
    container "" $ h2 "Cardano Native Tokens with Encrypted Redeeming Values"
    _ <- container "container-extra-margin-small" $ btnExternal "app.html" "button" "" $ text "Launch App"
    blank

communitySection :: MonadWidget t m => m ()
communitySection = section "" "" $ container "" $ do
    h4 "Join our community"
    divClass "div-our-resourses" $ ourResourses "83px"
    blank

featuresSection :: MonadWidget t m => m ()
featuresSection = section "Features" "" $ do
    container "" $ h3 "INTRODUCTION"
    explainer "How to mint?" "Send ADA into the protocol to mint a bundle of NFTs (aka ENCOINS). Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
    explainer "How to use?" "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded accounts system."
    explainer "How to redeem?" "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    _ <- container "container-extra-margin-small" $ elAttr "a" ("href" =: "app.html") $ btnExternal "app.html" "button" "" $ text "Launch App"
    blank
    where
        explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
            h4 txtTitle
            pClass "p-explainer" $ text txtExplainer

roadmapSection :: MonadWidget t m => m ()
roadmapSection = section "Roadmap" "" $ do
    container "" $ h3 "ROADMAP"
    roadmapItemLeft "01" True "ENCOINS Announced" $ text "ENCOINS protocol was announced in August 2022. The team released White Paper v0.1. The project received community funding through the ISPO and Project Catalyst."
    roadmapItemRight "02" False "Token Generation Event" $ do
        text "Shortly after our ISPO ends in February 2023, we will have the final ENCS token distribution. ENCS can then be generated and distributed using the algorithm presented "
        lnkInline "https://encoins-crypto.medium.com/fully-decentralized-token-distribution-on-cardano-9d7317d8de6" "here"
        text "."
    roadmapItemLeft  "03" False "Public Test" $ text "In March 2023, ENCOINS Public Test will commence. Users will be able to test the base dApp on Cardano testnets. In the second phase, we expect relayers to join in."
    roadmapItemRight "04" False "Mainnet Launch" $ text "After the Public Test, everything will be ready for the mainnet launch. Users can now mint, trade, and transfer ENCOINS using their wallet or ENCOINS Ledger."
    roadmapItemLeft  "05" False "ENSHARE Integrations" $ text "ENSHARE, our upcoming protocol for confidential data sharing, will be integrated with ENCOINS, enabling on-chain sharing of minting keys."
    roadmapItemRight "06" False "Ecosystem Integrations" $ text "We plan to integrate ENCOINS with selected Cardano Ecosystem projects, including wallets, DeFi, and NFT dApps. All potential partners are welcome!"
    roadmapItemLeft  "07" False "Native Assets Support" $ text "Currently, ENCOINS protocol only supports wrapping of ADA. This support will eventually be extended to all native assets on Cardano, including Cardano NFTs."
    where
        roadmapItemComplete = bool "text-roadmap-numbers" "text-roadmap-numbers text-roadmap-numbers-complete" 
        roadmapItemRight num b txtTitle tags = divClass "div-roadmap-item div-roadmap-item-right" $ do
            divClass (roadmapItemComplete b) $ text num
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" tags
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
        roadmapItemLeft num b txtTitle tags  = divClass "div-roadmap-item div-roadmap-item-left" $ do
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" tags
            divClass (roadmapItemComplete b) $ text num

partnersSection :: MonadWidget t m => m ()
partnersSection = section "Partners" "div-invisible" $ do
    container "" $ h3 "PARTNERS"
    divClass "div-partners" $ container "" blank