module ENCOINS.Website.Widgets.LandingPage
    ( landingPage
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Text (Text)
import Reflex.Dom
import Reflex.ScriptDependent (widgetHoldUntilDefined)

import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Basic
import ENCOINS.Website.Widgets.Basic
import ENCOINS.Website.Widgets.Resourses (ourResourses)
import JS.Website (scrollIntoView)

landingPage :: (MonadWidget t m) => Text -> m (Event t (Text, Text))
landingPage elemId = do
    -- Scrolling to a specific element
    ePB <- getPostBuild
    eWebpageLoaded <-
        updated
            <$> widgetHoldUntilDefined "scrollIntoView" ("js/Webpage.js" <$ ePB) blank blank
    performEvent_ (scrollIntoView elemId <$ eWebpageLoaded)

    -- Printing landing page
    titleSection
    communitySection
    featuresSection
    dexHunterSection
    roadmapSection
    partnersSection

    return never

titleSection :: (MonadWidget t m) => m ()
titleSection = section "" "" $ do
    container "" $ h4 "Protect your privacy with"
    container "" $ h1 "ENCOINS"
    container "" $ h2 "Cardano Native Tokens with Encrypted Redeeming Values"
    _ <-
        container "container-extra-margin-small" $
            btnExternal "https://app.encoins.io" "button" "" $
                text "Launch App"
    blank

communitySection :: (MonadWidget t m) => m ()
communitySection = section "Community" "" $ container "" $ do
    h4 "Join our community"
    divClass "div-our-resourses" $ ourResourses "83px"
    blank

featuresSection :: (MonadWidget t m) => m ()
featuresSection = section "Features" "" $ do
    container "" $ h3 "INTRODUCTION"
    explainer
        "How to mint?"
        "Send ADA into the protocol to mint a bundle of NFTs (aka ENCOINS). Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
    explainer
        "How to use?"
        "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded accounts system."
    explainer
        "How to redeem?"
        "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    _ <-
        container "container-extra-margin-small" $
            btnExternal "https://app.encoins.io" "button" "" $
                text "Launch App"
    blank
    where
        explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
            h4 txtTitle
            pClass "p-explainer" $ text txtExplainer

roadmapSection :: (MonadWidget t m) => m ()
roadmapSection = section "Roadmap" "" $ do
    container "" $ h3 "ROADMAP"
    roadmapItemLeft "01" False "Encoins V2 Testnet" $
        text
            "The Encoins V2 Testnet will introduce the ability to support any native Cardano asset, similar to $ADA in version 1. This development will allow for increased flexibility and enhanced user experience within the Encoins platform."
    roadmapItemRight "02" False "Trustless On-Ramp Solution" $
        text
            "Our OnRamp will feature a peer-to-peer (P2P) trustless and decentralized exchange of fiat and cryptocurrencies. This innovative solution will enable users to seamlessly transition between traditional currencies and digital assets, bridging the gap between conventional finance and the world of blockchain technology."
    roadmapItemLeft "03" False "Encoins V2 Mainnet" $
        text
            "Upon successful testing and refinement of our platform, we will launch Encoins V2 Mainnet. This upgrade will introduce integrations with other protocols, starting with popular wallets and decentralized exchanges (DExes). These strategic collaborations will further expand the accessibility and usability of our platform, empowering users with a diverse range of options for managing and exchanging their digital assets."
    where
        roadmapItemComplete =
            bool "text-roadmap-numbers" "text-roadmap-numbers text-roadmap-numbers-complete"
        roadmapItemRight num b txtTitle tags = divClass "div-roadmap-item div-roadmap-item-right" $ do
            divClass (roadmapItemComplete b) $ text num
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" tags
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
        roadmapItemLeft num b txtTitle tags = divClass "div-roadmap-item div-roadmap-item-left" $ do
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
            divClass "div-roadmap-item-description" $ do
                h5 txtTitle
                divClass "p-roadmap-item" tags
            divClass (roadmapItemComplete b) $ text num

partnersSection :: (MonadWidget t m) => m ()
partnersSection = section "Partners" "div-invisible" $ do
    container "" $ h3 "PARTNERS"
    divClass "div-partners" $ container "" blank

dexHunterSection :: (MonadWidget t m) => m ()
dexHunterSection =
    section "buy-encoins" "" $ do
        container "" $ h3 "Buy ENCS"
        divClass "main-DexHunter_Container" $
            elAttr "div" ("id" =: "dexhunter-root") blank
