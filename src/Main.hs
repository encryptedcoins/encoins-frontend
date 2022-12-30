{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.List.Index (indexed)
import Data.Text (Text, pack, replace, take, toLower)
import qualified JS
import Reflex.Dom
import Text.Printf (printf)
import Widgets.App (appWidget)
import Widgets.Utils (toText)

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

{- FOURMOLU DISABLE -}
headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "ENCOINS Wallet"
  el "style" $
    text
      ".wf-force-outline-none[tabindex=\"-1\"]:focus{outline:none;}"
  meta $ "charset" =: "utf-8"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "og:title"
  meta $ "content" =: "ENCOINS Wallet" <> "property" =: "twitter:title"
  meta $
    "content" =: "width=device-width, initial-scale=1"
      <> "name" =: "viewport"
  stylesheet "css/normalize.css"
  stylesheet "css/webflow.css"
  stylesheet "css/encs-3bb243bf2a8520d5a66d-f872d515808be.webflow.css"
  elAttr
    "link"
    ( "rel" =: "stylesheet" <> "media" =: "all"
        <> "href"
          =: "http://fonts.googleapis.com/css?family=Poppins:400,500,600,700,800|Inter:400,500,700|Mulish:400,500"
    )
    blank
  elAttr
    "link"
    ( "href" =: "images/favicon.png" <> "rel" =: "shortcut icon"
        <> "type" =: "image/x-icon"
    )
    blank
  elAttr
    "link"
    ("href" =: "images/webclip.png" <> "rel" =: "apple-touch-icon")
    blank

  eWebFontLoaded <-
    domEvent Load . fst
      <$> elAttr'
        "script"
        ( "src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"
            <> "type" =: "text/javascript"
        )
        blank
  eCSLLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/CSL.js" <> "type" =: "text/javascript") blank
  eWebpageLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/Webpage.js" <> "type" =: "text/javascript") blank
  eEd25519Loaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/noble-ed25519.js" <> "type" =: "text/javascript") blank
  
  dWebFontLoaded <- holdDyn False (True <$ eWebFontLoaded)
  dCSLLoaded <- holdDyn False (True <$ eCSLLoaded)
  dWebpageLoaded <- holdDyn False (True <$ eWebpageLoaded)
  dEd25519Loaded <- holdDyn False (True <$ eEd25519Loaded)
  let eScriptsLoaded = ffilter (== True) $ updated $ foldl (zipDynWith (&&)) (pure True) [dWebFontLoaded, dWebpageLoaded, dCSLLoaded, dEd25519Loaded]

  performEvent_ (JS.runHeadScripts <$ eScriptsLoaded)
 where
  meta attr = elAttr "meta" attr blank
  stylesheet href =
    elAttr
      "link"
      ( "href" =: href <> "rel" =: "stylesheet"
          <> "type" =: "text/css"
      )
      blank

{- FOURMOLU ENABLE -}
bodyWidget :: (MonadWidget t m) => m ()
bodyWidget = mdo
  navbarWidget
  ispoWidget
  landingWidget
  footerWidget

navbarWidget :: MonadWidget t m => m ()
navbarWidget = elAttr "div" ("class" =: "navbar") do
  elClass "p" "top-logo" $ elAttr "img" ("class" =: "group-1" <> "src" =: "images/icon.svg" <> "width" =: "83px" <> "height" =: "83px" <> "alt" =: "Group 1") blank
  divClass "nav-links" $ do
    mapM_ (mkLink "poppins-normal-white-20px") (init navLinks)
    elClass "a" "poppins-extra-bold-white-18px white-paper-button" $ text "White Paper"

data Info = Info Text Text

introInfos :: [Info]
introInfos =
  [ Info "How to mint?" $
      "Send ADA into the protocol to mint a bundle of NFT's (aka ENCOINS). "
        <> "Each token contains an encrypted redeeming value known only to you. The total redeeming value is equal to the ADA provided."
  , Info "How to use?" $
      "ENCOINS can be used as any other native asset on Cardano: they can be traded, "
        <> "gifted, or used in other DeFi protocols that support them. They can also be used in ENCOINS Ledger, our upcoming shielded account system."
  , Info "How to redeem?" $
      "ENCOINS can be burned to receive their redeeming ADA value back. "
        <> "Only the user who knows its minting key can redeem an ENCOINS token."
  ]

lorem :: Info
lorem = Info "Developing" "A Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's  standard dummy text ever since the 1500s, "

roadmapInfos :: [Info]
roadmapInfos = replicate 5 lorem

landingWidget :: (MonadWidget t m) => m ()
landingWidget = elClass "div" "info" $ do
  divClass "rectangle-26" blank
  divClass "slogan" $ do
    divClass "up poppins-normal-pink-swan-28px" $ text "Protect your privacy with"
    divClass "mid poppins-extra-bold-white-65px" $ text "ENCOINS"
    divClass "down poppins-bold-white-36px" $ text "Cardano Native Tokens with Encrypted Reedeming Values"
  _ <- elAttr "a" ("class" =: "white-launch-button poppins-extra-bold-woodsmoke-18px") $ button "Launch App"
  divClass "poppins-normal-white-28px join-our" $ text "Join our community!"
  divClass "icons-big" $
    mapM_ (mkLinkIcon (85, 85)) iconInfos
  divClass "introduction" $ do
    elClass "p" "poppins-extra-bold-white-36px intro-head" $ text "INTRODUCTION"
    mapM_ mkIItem introInfos
    _ <- elAttr "a" ("class" =: "black-launch-button poppins-extra-bold-white-18px") $ button "Launch App"
    blank
  divClass "roadmap" $ do
    elClass "p" "poppins-extra-bold-white-36px roadmap-head" $ text "ROADMAP"
    mapM_ mkRItem (indexed roadmapInfos)
    _ <- elAttr "a" ("class" =: "black-launch-button poppins-extra-bold-white-18px") $ button "Let's See More"
    blank
  divClass "partners" $
    elClass "p" "poppins-bold-bianca-50px participate" $ text "Participate in our ISPO"
 where
  mkIItem (Info _head _text) = divClass "intro-item" $ do
    elClass "p" ("poppins-normal-pink-swan-24px head-intro-" <> Data.Text.take 5 _head) $ text _head
    elClass "p" ("poppins-normal-white-36px text-intro-" <> Data.Text.take 5 _text) $ text _text

mkRItem :: (MonadWidget t m) => (Int, Info) -> m ()
mkRItem (ind, Info _head _text) = case (ind + 1) `mod` 2 of
  0 ->
    divClass "roadmap-item-0 roadmap-item" $ do
      groomImg
      mkText
      index
  1 ->
    divClass "roadmap-item-1 roadmap-item" $ do
      index
      mkText
      groomImg
  _ -> undefined
 where
  groomImg :: MonadWidget t m1 => m1 ()
  groomImg =
    elAttr
      "img"
      ( "src" =: "images/grommet-tech.svg"
          <> "width" =: "72"
          <> "height" =: "72"
          <> "alt" =: "Grommet Tech"
          <> "class" =: "grommet-icon"
      )
      blank
  mkText :: MonadWidget t m1 => m1 ()
  mkText = divClass "roadmap-item-text" $ do
    elClass "a" ("poppins-bold-white-24px head-roadmap-" <> Data.Text.take 5 _head) $ text _head
    elClass "a" ("poppins-normal-pink-swan-18px text-roadmap-1 text-roadmap-" <> Data.Text.take 5 _text) (text _text)
  index :: MonadWidget t m1 => m1 ()
  index = elClass "a" ("poppins-bold-black-129px roadmap-ind roadmap-ind-" <> toText ind) $ text (pack $ printf "%02d" (ind + 1))

ispoWidget :: MonadWidget t m => m ()
ispoWidget = divClass "ispo-info" $ do
  elClass "p" "poppins-bold-bianca-80px ispo-head" $ text "Initial Stake Pool Offering"
  elClass "p" "poppins-normal-bianca-65px ispo-head-2" $ text "Participate in the ISPO to support our project!"
  elClass "p" "ispo-text" $
    mapM_
      ( \textBlock -> el "p" $ do
          mapM_ mkFormat textBlock
      )
      [
        [ ("poppins-medium-white-20px", " Initial stake pool offering (ISPO)")
        ,
          ( "poppins-normal-pink-swan-20px"
          , " is a novel method to distribute a protocol's"
              <> " utility tokens to the community using the Cardano blockchain. It ensures a high degree of decentralization of the token"
              <> "ownership and simultaneously supports the project's development."
          )
        ]
      ,
        [
          ( "poppins-normal-pink-swan-20px"
          , "Currently, we are distributing a total of 10 000 000 ENCS tokens (from the total supply of 15 000 000 ENCS) to delegators of"
              <> " our very own stake pool with the ticker symbol "
          )
        , ("poppins-medium-white-20px", "[CMIX]")
        ,
          ( "poppins-normal-pink-swan-20px"
          , ". Our ISPO, initially started on Epoch 319 (early February 2022) as CardMix ISPO,"
              <> " will last until all tokens are assigned. To participate in the token distribution, users delegate their ADA to our stake pool [CMIX]. "
              <> "The ENCS rewards are calculated for every epoch individually, so users may decide to participate for any number of epochs. "
              <> "Our pool is also a part of "
          )
        , ("poppins-medium-white-20px", "Cardano Single Pool Alliance (CSPA)")
        , ("poppins-normal-pink-swan-20px", ".")
        ]
      ,
        [ ("poppins-normal-pink-swan-20px", "Our pool:")
        , ("poppins-medium-white-20px", "pool1pmeetaqhlsdc5yj8snp8j46hprs9mzghxcl63vch95trz3xwplnIn hex format: 0ef395f417fc1b8a124784c279575708e05d8917363fa8b3172d1631")
        ]
      ,
        [ ("poppins-normal-pink-swan-20px", "Check the pool on third-party aggregators: ")
        , ("poppins-medium-white-20px", "cexplorer.io")
        , ("poppins-normal-pink-swan-20px", ", ")
        , ("poppins-medium-white-20px", "adapools.org")
        , ("poppins-normal-pink-swan-20px", ", ")
        , ("poppins-medium-white-20px", "pooltool.io")
        ,
          ( "poppins-normal-pink-swan-20px"
          , ".You can delegate your ADA to a stake pool in your Cardano"
              <> " wallet app. It is completely safe, as your funds never leave your wallet. Enter CMIX in the pool search textbox inside your wallet app "
              <> "to find our stake pool. To be sure you are delegating to the right pool, check the pool ID."
          )
        ]
      ]
  divClass "reward-calculator" $ do
    elClass "p" "poppins-extra-bold-white-35px reward-calculator-head" $ text "REWARDS CALCULATOR"
    elClass "p" "poppins-normal-pink-swan-20px reward-calculator-text" $
      text $
        "Participants of the ISPO can track their accumulated rewards using the calculator below. Enter "
          <> "your stake key to see how many ENCS tokens you have secured so far. The information is updated every epoch (5 days)."
    divClass "group-3" $ do
      divClass "group-4" $ do
        _ <- elAttr "a" ("class" =: "poppins-white-normal-18px subscribe-input") $
            inputElement $
              def
                & initialAttributes
                  .~ "class" =: "address-rewards-input-field poppins-extra-bold-mercury-18px"
                    <> "type" =: "text"
                    <> "placeholder" =: "stake6251g23y2ey62geyhbnjk23ki0213i"
        elClass "p" "poppins-semi-bold-white-18px reward-result" $ text "0.0 ENCS"
      divClass "group-5" $ do
        elClass "p" "poppins-semi-bold-white-18px all-distributed" $ text "Distributed in ISPO:"
        elClass "p" "poppins-white-normal-18px all-distributed-amount" $ text "7 145 560 ENCS (out of 10 000 000 ENCS)."
    divClass "reward-conditions" $ do
      elClass "p" "reward-conditions" $ do
        elClass "span" "poppins-normal-pink-swan-20px" $
          text $
            "Each epoch, we distribute between 120 000 and 600 000 tokens depending on the pool saturation. "
              <> "The per-ADA rewards are higher when the saturation is low. 90% of ENCS tokens are distributed proportionally to your delegation. "
              <> "The rest is distributed through a lottery. Every ISPO participant with at least 1000 ADA stake is eligible. You can check the lottery details "
        elClass "span" "poppins-medium-white-20px" $ text "here"
        elClass "span" "poppins-normal-pink-swan-20px" $ text "."
  divClass "winners" $ do
    elClass "p" "winners-head poppins-extra-bold-white-35px" $ text "Epoch 373 lottery winners:"
    elClass "p" "winners-1 poppins-normal-pink-swan-20px" $ text "1) stake1uxuemrqdt50dmlw5tcdmqjmevg5kclvywga8zuea37rlj8c93vhnu"
    elClass "p" "winners-2 poppins-normal-pink-swan-20px" $ text "2) stake1u9p34a6cqhf4j7l6cywfjns6tqfsstvqel5jpkyay4uptugeggapx"
    elClass "p" "winners-3 poppins-normal-pink-swan-20px" $ text "3) stake1u960qt2qd4lu5nfu2nn43v8k8gp239drwtl6x5wsvl5vgzc3u92yk"
    elClass "p" "winners-conditions" $ do
      elClass "span" "poppins-normal-pink-swan-20px" $ text "The winners secure additional 4000 ENCS. "
      elClass "span" "poppins-medium-white-20px" $ text "Congratulations!!!"
  elClass "p" "poppins-extra-bold-bianca-50px ispo-end" $ text "Cardano Native Tokens with Encrypted Redeeming Values"
 where
  mkFormat :: MonadWidget t m => (Text, Text) -> m ()
  mkFormat (font, t) = elClass "span" font $ text t

data LinkInfo = LinkInfo
  { linkHref :: Text
  , linkText :: Text
  }

navLinks :: [LinkInfo]
navLinks =
  [ LinkInfo "" "Home"
  , LinkInfo "" "ISPO"
  , LinkInfo "" "Governance"
  , LinkInfo "" "White Paper"
  ]

intraLinks :: [LinkInfo]
intraLinks =
  [ LinkInfo "" "Features"
  , LinkInfo "" "Roadmap"
  , LinkInfo "" "Partners"
  ]

mkLink :: MonadWidget t m => Text -> LinkInfo -> m ()
mkLink textClass (LinkInfo href lText) =
  elAttr "a" ("href" =: href <> "class" =: (textClass <> " " <> textClass <> "-" <> toLower (replace " " "-" lText))) $ text lText

data IconInfo = IconInfo
  { classPrefix :: Text
  , iconSrc :: Text
  , iconAlt :: Text
  , link :: Text
  }

mkLinkIcon :: MonadWidget t m => (Int, Int) -> IconInfo -> m ()
mkLinkIcon (width, height) (IconInfo classPrefix iconSrc iconAlt href) =
  elAttr "a" ("href" =: href {-<> "class" =: "w-inline-block"-}) $
    elAttr
      "img"
      ( "src" =: iconSrc
          <> "width" =: toText width
          <> "height" =: toText height
          <> "alt" =: iconAlt
          <> "class" =: (classPrefix <> "-small-icon")
      )
      blank

iconInfos :: [IconInfo]
iconInfos =
  map
    (\(a, b, c, d) -> IconInfo a b c d)
    [ ("twitter", "images/twitter.svg", "Twitter", "https://twitter.com/ENCOINS1")
    , ("discord", "images/discord.svg", "Discord", "https://discord.gg/Q3gPP87Tcw")
    , ("medium", "images/Medium.svg", "Medium", "http://encoins-crypto.medium.com/")
    , ("email", "images/email.svg", "E-Mail", "")
    , ("telegram", "images/telegram.svg", "Telegram", "https://t.me/encoins_io")
    , ("docs", "images/Docs.svg", "Source Docs", "https://encoins.gitbook.io/source-code-docs")
    , ("github", "images/GitHub.svg", "GitHub", "https://github.com/encryptedcoins")
    , ("youtube", "images/Youtube.svg", "YouTube", "")
    ]

footerWidget :: MonadWidget t m => m ()
footerWidget = divClass "footer" $ do
  leftPart
  elAttr "img" ("class" =: "line-2-1" <> "height" =: "580px" <> "src" =: "images/line-2.svg" <> "alt" =: "Line 2") blank
  rightPart
 where
  leftPart :: MonadWidget t m => m ()
  leftPart = divClass "flex-col-1" $ do
    elAttr "img" ("class" =: "group-127" <> "src" =: "images/icon.svg" <> "alt" =: "Group 127") blank
    elClass "p" "valign-tex-middle poppins-normal-white-18px text-about" $
      text $
        "A Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard\n"
          <> "dummy text ever since the 1500s."
    divClass "our-resources poppins-bold-bianca-24px" $ text "Our Resources"
    icons
    divClass "all-rights-reserved poppins-normal-white-18px" $ text "All rights reserved@2022"
  rightPart :: MonadWidget t m => m ()
  rightPart = divClass "flex-col-2" $ do
    divClass "flex-col-3" $ do
      divClass "poppins-bold-bianca-32px subscribe-for-updates" $ text "Subscribe for updates"
      emailForm
    elAttr "img" ("class" =: "line-1" <> "width" =: "576" <> "src" =: "images/line-3.svg" <> "alt" =: "Line 3") blank
    divClass "flex-row-2" $ do
      divClass "flex-col-4" $ do
        elClass "span" "poppins-extra-bold-bianca-24px" $ text "About"
        mapM_ (mkLink "other-link") navLinks
      divClass "flex-col-5" $ do
        elClass "span" "poppins-extra-bold-bianca-24px" $ text "Links"
        mapM_ (mkLink "other-link") intraLinks
  icons :: MonadWidget t m => m ()
  icons =
    divClass "icons-small" $
      mapM_ (mkLinkIcon (30, 30)) iconInfos

  emailForm :: MonadWidget t m => m ()
  emailForm = elClass "form" "email-form" $ do
    _ <-
      elAttr "a" ("class" =: "subscribe-input") $
        inputElement $
          def
            & initialAttributes
              .~ "class" =: "subscribe-input-field poppins-extra-bold-mercury-18px"
                <> "type" =: "text"
                <> "placeholder" =: "E-mail"

    _ <- elAttr "a" ("class" =: "subscribe-button poppins-extra-bold-woodsmoke-18px") $ button "Subscribe"
    pure ()