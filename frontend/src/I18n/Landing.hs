{-# LANGUAGE OverloadedStrings #-}

module I18n.Landing where

import I18n.Common.I18n
import Data.Text (Text)

data LandMessage
    = MenuHome
    | MenuApp
    | MenuDao
    | MenuBuyEncs
    | MenuPaper
    | TitleProtect
    | TitleDescription
    | ButtonLaunchApp
    | JoinCommunity
    | Introduction
    | HowMint
    | HowMintText
    | HowUse
    | HowUseText
    | HowRedeam
    | HowRedeamText
    | RoadMap
    | RoadMapTestnent
    | RoadMapTestnentText
    | RoadMapTrustless
    | RoadMapTrustlessText
    | RoadMapMainnet
    | RoadMapMainnetText
    | FootEncsPolicyId
    | FootResourses
    | FootAbout
    | FootFeatures
    | FootLinks
    deriving stock (Eq)

instance HasI18n Locale LandMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showLandMessageEn t
    Locale_ZH -> showLandMessageEn t -- TODO: update it

showLandMessageEn :: LandMessage -> Text 
showLandMessageEn = \case 
    MenuHome -> "Home"
    MenuApp -> "APP"
    MenuDao -> "Dao"
    MenuBuyEncs -> "Buy ENCS"
    MenuPaper -> "White paper"
    TitleProtect -> "Protect your privacy with"
    TitleDescription -> "Cardano Native Tokens with Encrypted Redeeming Values"
    ButtonLaunchApp -> "Launch App"
    JoinCommunity -> "Join our community"
    Introduction -> "Introduction"
    HowMint -> "How to mint?"
    HowMintText ->
        "Send ADA into the protocol to mint a bundle of NFTs (aka ENCOINS). Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
    HowUse -> "How to use?"
    HowUseText ->
        "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded accounts system."
    HowRedeam -> "How to redeem?"
    HowRedeamText ->
        "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    RoadMap -> "Roadmap"
    RoadMapTestnent -> "Encoins V2 Testnet"
    RoadMapTestnentText ->
        "he Encoins V2 Testnet will introduce the ability to support any native Cardano asset, similar to $ADA in version 1. This development will allow for increased flexibility and enhanced user experience within the Encoins platform."
    RoadMapTrustless -> "Trustless On-Ramp Solution"
    RoadMapTrustlessText ->
        "Our OnRamp will feature a peer-to-peer (P2P) trustless and decentralized exchange of fiat and cryptocurrencies. This innovative solution will enable users to seamlessly transition between traditional currencies and digital assets, bridging the gap between conventional finance and the world of blockchain technology."
    RoadMapMainnet -> "Encoins V2 Mainnet"
    RoadMapMainnetText ->
        "Upon successful testing and refinement of our platform, we will launch Encoins V2 Mainnet. This upgrade will introduce integrations with other protocols, starting with popular wallets and decentralized exchanges (DExes). These strategic collaborations will further expand the accessibility and usability of our platform, empowering users with a diverse range of options for managing and exchanging their digital assets."
    FootEncsPolicyId ->
        "ENCOINS is a decentralized private accounts and payments protocol on the Cardano blockchain. ENCS utility token policyID:"
    FootResourses -> "Our Resourses"
    FootAbout -> "About"
    FootFeatures -> "Features"
    FootLinks -> "Links"
