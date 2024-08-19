{-# LANGUAGE OverloadedStrings #-}

module I18n.Landing where

import Data.Text (Text)

import Common.Utility (column, space)
import I18n.Common.I18n

data LandMessage
    = MenuApp
    | MenuDao
    | MenuPaper
    | Home
    | BuyEncs
    | TitleProtect
    | TitleDescription
    | ButtonLaunchApp
    | JoinCommunity
    | Introduction
    | HowMint
    | HowMintText
    | HowUse
    | HowUseText
    | HowRedeem
    | HowRedeemText
    | RoadMap
    | RoadMapTestnet
    | RoadMapTestnetText
    | RoadMapTrustless
    | RoadMapTrustlessText
    | RoadMapMainnet
    | RoadMapMainnetText
    | FootEncsDescription
    | FootPolicyId
    | FootResources
    | FootAbout
    | FootFeatures
    | FootLinks
    | Partners
    | Rights
    deriving stock (Eq, Show)

instance HasI18n Locale LandMessage Text where
    localizeWith locale t = case locale of
        Locale_EN -> showLandMessageEn t
        Locale_RU -> showLandMessageRu t

showLandMessageEn :: LandMessage -> Text
showLandMessageEn = \case
    MenuApp -> "APP"
    MenuDao -> "Dao"
    MenuPaper -> "White paper"
    Home -> "Home"
    BuyEncs -> "Buy ENCS"
    TitleProtect -> "Protect your privacy with"
    TitleDescription -> "Cardano Native Tokens with Encrypted Redeeming Values"
    ButtonLaunchApp -> "Launch App"
    JoinCommunity -> "Join our community"
    Introduction -> "INTRODUCTION"
    HowMint -> "How to mint?"
    HowMintText ->
        "Send ADA into the protocol to mint a bundle of NFTs (aka ENCOINS). Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
    HowUse -> "How to use?"
    HowUseText ->
        "ENCOINS can be used as any other native asset on Cardano: they can be traded, gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded accounts system."
    HowRedeem -> "How to redeem?"
    HowRedeemText ->
        "ENCOINS can be burned to receive their redeeming ADA value back. Only the user who knows its minting key can redeem an ENCOINS token."
    RoadMap -> "Roadmap"
    RoadMapTestnet -> "Encoins V2 Testnet"
    RoadMapTestnetText ->
        "The Encoins V2 Testnet will introduce the ability to support any native Cardano asset, similar to $ADA in version 1. This development will allow for increased flexibility and enhanced user experience within the Encoins platform."
    RoadMapTrustless -> "Trustless On-Ramp Solution"
    RoadMapTrustlessText ->
        "Our OnRamp will feature a peer-to-peer (P2P) trustless and decentralized exchange of fiat and cryptocurrencies. This innovative solution will enable users to seamlessly transition between traditional currencies and digital assets, bridging the gap between conventional finance and the world of blockchain technology."
    RoadMapMainnet -> "Encoins V2 Mainnet"
    RoadMapMainnetText ->
        "Upon successful testing and refinement of our platform, we will launch Encoins V2 Mainnet. This upgrade will introduce integrations with other protocols, starting with popular wallets and decentralized exchanges (DExes). These strategic collaborations will further expand the accessibility and usability of our platform, empowering users with a diverse range of options for managing and exchanging their digital assets."
    FootEncsDescription ->
        "ENCOINS is a decentralized private accounts and payments protocol on the Cardano blockchain." <> space
    FootPolicyId -> "ENCS utility token policyID" <> column <> space
    FootResources -> "Our Resources"
    FootAbout -> "About"
    FootFeatures -> "Features"
    FootLinks -> "Links"
    Partners -> "PARTNERS"
    Rights -> "All rights reserved"


showLandMessageRu :: LandMessage -> Text
showLandMessageRu = \case
    MenuApp -> "APP"
    MenuDao -> "Dao"
    MenuPaper -> "White paper"
    Home -> "Главная"
    BuyEncs -> "Купить ENCS"
    TitleProtect -> "Защитите вашу приватность с"
    TitleDescription -> "Токены на блокчейн Cardano с зашифрованными выкупными значениями"
    ButtonLaunchApp -> "Запуск приложения"
    JoinCommunity -> "Присоединяйтесь к нашему сообществу"
    Introduction -> "ВВЕДЕНИЕ"
    HowMint -> "Как чеканить?"
    HowMintText ->
        "Отправьте ADA в протокол для создания NFT токенов (также известных как ENCOINS). Каждый токен содержит зашифрованное значение выкупа, известное только вам. Общая стоимость выкупа равна сумме предоставленных ADA."
    HowUse -> "Как использовать?"
    HowUseText ->
        "ENCOINS можно использовать как любой другой нативный актив в сети Cardano: их можно торговать, дарить или использовать в других DeFi-протоколах, которые их поддерживают. Их также можно использовать в ENCOINS Ledger, нашей предстоящей системе защищённых счетов."
    HowRedeem -> "Как выкупать?"
    HowRedeemText ->
        "ENCOINS могут быть сожжены, чтобы получить обратно их обменный ADA. Только пользователь, который знает ключ чеканки токена, может погасить токен ENCOINS."
    RoadMap -> "Дорожная карта"
    RoadMapTestnet -> "Тестовая сеть Encoins V2"
    RoadMapTestnetText ->
        "Тестовая сеть Encoins V2 предоставит возможность поддерживать любые нативные активы Cardano, подобно $ADA в версии 1. Это развитие позволит увеличить гибкость и улучшить пользовательский опыт на платформе Encoins."
    RoadMapTrustless -> "Безопасный шлюз для входа"
    RoadMapTrustlessText ->
        "Наш входной шлюз (OnRamp) будет включать в себя доверительный одноранговый (P2P) и децентрализованный обмен фиатных и криптовалют. Это инновационное решение позволит пользователям легко переходить между традиционными валютами и цифровыми активами, устраняя разрыв между традиционными финансами и миром блокчейн-технологий."
    RoadMapMainnet -> "Главная сеть Encoins V2"
    RoadMapMainnetText ->
        "После успешного тестирования и доработки нашей платформы мы запустим Encoins V2 в главное сети блокчейна Cardano. Это обновление обеспечит интеграции с другими протоколами, начиная с популярных кошельков и децентрализованных бирж (DEx). Эти стратегические коллаборации позволят значительно расширить доступность и функциональность нашей платформы, предоставляя пользователям широкий выбор опций для управления и обмена своими цифровыми активами."
    FootEncsDescription ->
        "ENCOINS — это децентрализованный протокол для приватных счетов и платежей на блокчейне Cardano." <> space
    FootPolicyId -> "PolicyID ENCS токена" <> column <> space
    FootResources -> "Наши ресурсы"
    FootAbout -> "О нас"
    FootFeatures -> "Преимущества"
    FootLinks -> "Ссылки"
    Partners -> "Партнеры"
    Rights -> "All rights reserved"
