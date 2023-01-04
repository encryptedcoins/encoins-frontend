module ENCOINS.Website.Widgets.LandingPage (landingPage) where

import           Data.Text                         (Text)
import           Reflex.Dom

import           ENCOINS.Website.Widgets.Basic
import           ENCOINS.Website.Widgets.Resourses (ourResourses)
import Data.Bool (bool)
import Reflex.ScriptDependent (widgetHoldUntilDefined)
import JS.WebPage (scrollIntoView, logInfo)

landingPage :: MonadWidget t m => Text -> m (Event t (Text, Text))
landingPage idFocus = do
    ePB <- getPostBuild
    eWebpageLoaded <- updated <$> widgetHoldUntilDefined "scrollIntoView" ("js/Webpage.js" <$ ePB) blank blank
    performEvent_ (scrollIntoView idFocus <$ eWebpageLoaded)

    titleSection
    communitySection
    featuresSection
    roadmapSection
    partnersSection
    ispoCallSection

titleSection :: MonadWidget t m => m ()
titleSection = section "" "" $ do
    container "" $ h4 "Protect your privacy with"
    container "" $ h1 "ENCOINS"
    container "" $ h2 "Cardano Native Tokens with Encrypted Redeeming Values"
    _ <- container "container-extra-margin-small" $ btn "button-disabled" "Launch App"
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
    explainer "How to use?" "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded account system."
    explainer "How to redeem?" "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    _ <- container "container-extra-margin-small" $ btn "button-disabled" "Launch App"
    blank
    where
        explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
            h4 txtTitle
            pClass "p-explainer" $ text txtExplainer

roadmapSection :: MonadWidget t m => m ()
roadmapSection = section "Roadmap" "" $ do
    container "" $ h3 "ROADMAP"
    roadmapItemLeft "01" True "ENCOINS Announced" "ENCOINS protocol was announced in August 2023. The team released White Paper v0.1. The project recieved community funding through the ISPO and Project Catalyst."
    roadmapItemRight "02" False "Token Generation Event" "Shortly after our ISPO ends in February 2023, we will have the final ENCS token distribution. ENCS can then be generated and distributed using the algorithm presented here."
    roadmapItemLeft  "03" False "Public Test" "In March 2023, ENCOINS Public Test will commence. Users will be able to test the base dApp on Cardano testnets. In the second phase, we expect relayers to join in."
    roadmapItemRight "04" False "Mainnet Launch" "After the Public Test, everything will be ready for the mainnet launch. Users can now mint, trade, and transfer ENCOINS using their wallet or ENCOINS Ledger."
    roadmapItemLeft  "05" False "ENSHARE Integrations" "ENSHARE, our upcoming protocol for confidential data sharing, will be integrated with ENCOINS, enabling on-chain sharing of minting keys."
    roadmapItemRight "06" False "Ecosystem Integrations" "We plan to integrate ENCOINS with selected Cardano Ecosystem projects, including wallets, DeFi, and NFT dApps. All potential partners are wellcome!"
    roadmapItemLeft  "07" False "Native Assets Support" "Currently, ENCOINS protocol only supports wrapping of ADA. This support will be eventually extended to all native assets on Cardano, including Cardano NFTs."
    where
        roadmapItemComplete = bool "text-roadmap-numbers" "text-roadmap-numbers text-roadmap-numbers-complete" 
        roadmapItemRight num b txtTitle txtDesc = divClass "div-roadmap-item div-roadmap-item-right" $ do
            divClass (roadmapItemComplete b) $ text num
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" $ text txtDesc
            image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
        roadmapItemLeft num b txtTitle txtDesc  = divClass "div-roadmap-item div-roadmap-item-left" $ do
            image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" $ text txtDesc
            divClass (roadmapItemComplete b) $ text num
            
ispoCallSection :: MonadWidget t m => m (Event t (Text, Text))
ispoCallSection = section "" "" $ container "" $ do
     (e, _) <- elAttr' "a" ("href" =: "#Navbar" <> "class" =: "h3") $ text "Participate in the ISPO to support our project!"
     return $ ("ISPO", "Navbar") <$ domEvent Click e

partnersSection :: MonadWidget t m => m ()
partnersSection = section "Partners" "div-invisible" $ do
    container "" $ h3 "PARTNERS"
    divClass "div-partners" $ container "" blank