module ENCOINS.Website.Widgets.LandingPage
    ( landingPage
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Text (Text)
import Reflex.Dom
import Reflex.ScriptDependent (widgetHoldUntilDefined)

import Common.Events
import Common.Reflex.Dom.Extra (textLocale)
import ENCOINS.Common.Widgets.Basic
import ENCOINS.Website.Widgets.Basic
import ENCOINS.Website.Widgets.Resources (ourResources)
import I18n.I18n (App)
import qualified I18n.Landing as I18n
import JS.Website (scrollIntoView)

landingPage :: (App t m) => Text -> m (Event t (Text, Text))
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

titleSection :: (App t m) => m ()
titleSection = section "" "" $ do
    container "" $ h4Locale I18n.TitleProtect
    container "" $ h1 "ENCOINS"
    container "" $ h2Locale I18n.TitleDescription
    _ <-
        container "container-extra-margin-small" $
            btnExternal "https://app.encoins.io" "button" "" $
                textLocale I18n.ButtonLaunchApp
    blank

communitySection :: (App t m) => m ()
communitySection = section "Community" "" $ container "" $ do
    h4Locale I18n.JoinCommunity
    divClass "div-our-resourses" $ ourResources "83px"
    blank

featuresSection :: (App t m) => m ()
featuresSection = section "Features" "" $ do
    container "" $ h3Locale I18n.Introduction
    explainer
        I18n.HowMint
        I18n.HowMintText
    explainer
        I18n.HowUse
        I18n.HowUseText
    explainer
        I18n.HowRedeem
        I18n.HowRedeemText
    _ <-
        container "container-extra-margin-small" $
            btnExternal "https://app.encoins.io" "button" "" $
                textLocale I18n.ButtonLaunchApp
    blank
    where
        explainer txtTitle txtExplainer = container "" $ divClass "div-explainer" $ do
            h4Locale txtTitle
            pClass "p-explainer" $ textLocale txtExplainer

roadmapSection :: (App t m) => m ()
roadmapSection = section "Roadmap" "" $ do
    container "" $ h3Locale I18n.RoadMap
    roadmapItemLeft "01" False I18n.RoadMapTestnet $ textLocale I18n.RoadMapTestnetText
    roadmapItemRight "02" False I18n.RoadMapTrustless $ textLocale I18n.RoadMapTrustlessText
    roadmapItemLeft "03" False I18n.RoadMapMainnet $ textLocale I18n.RoadMapMainnetText
    where
        roadmapItemComplete =
            bool "text-roadmap-numbers" "text-roadmap-numbers text-roadmap-numbers-complete"
        roadmapItemRight num b txtTitle tags = divClass "div-roadmap-item div-roadmap-item-right" $ do
            divClass (roadmapItemComplete b) $ text num
            divClass "div-roadmap-item-description" $ do
                h5Locale txtTitle
                divClass "p-roadmap-item" tags
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
        roadmapItemLeft num b txtTitle tags = divClass "div-roadmap-item div-roadmap-item-left" $ do
            void $ image "Roadmap-Icon.svg" "image-roadmap-item" "100px"
            divClass "div-roadmap-item-description" $ do
                h5Locale txtTitle
                divClass "p-roadmap-item" tags
            divClass (roadmapItemComplete b) $ text num

partnersSection :: (App t m) => m ()
partnersSection = section "Partners" "div-invisible" $ do
    container "" $ h3Locale I18n.Partners
    divClass "div-partners" $ container "" blank

dexHunterSection :: (App t m) => m ()
dexHunterSection =
    section "buy-encoins" "" $ do
        container "" $ h3Locale I18n.BuyEncs
        divClass "main-DexHunter_Container" $
            elAttr "div" ("id" =: "dexhunter-root") blank
