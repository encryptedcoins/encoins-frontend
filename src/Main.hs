{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified JS
import Reflex.Dom
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
  eEncoinsLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/ENCOINS.js" <> "type" =: "text/javascript") blank
  eCSLLoaded <- domEvent Load . fst <$> elAttr' "script" ("src" =: "js/CSL.js" <> "type" =: "text/javascript") blank

  dWebFontLoaded <- holdDyn False (True <$ eWebFontLoaded)
  dEncoinsLoaded <- holdDyn False (True <$ eEncoinsLoaded)
  dCSLLoaded <- holdDyn False (True <$ eCSLLoaded)
  let eScriptsLoaded = ffilter (== True) $ updated $ foldl (zipDynWith (&&)) (pure True) [dWebFontLoaded, dEncoinsLoaded, dCSLLoaded]

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

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  navbarWidget
  logoWidget
  appWidget
  footerWidget

navbarWidget :: MonadWidget t m => m ()
navbarWidget = blank

{- FOURMOLU ENABLE -}

logoWidget :: MonadWidget t m => m ()
logoWidget = blank

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

mkLink :: MonadWidget t m => LinkInfo -> m ()
mkLink (LinkInfo href lText) =
  elAttr "a" ("href" =: href <> "class" =: "poppins-normal-white-20px other-link") $ text lText

data IconInfo = IconInfo
  { classPrefix :: Text
  , iconSrc :: Text
  , iconAlt :: Text
  , link :: Text
  }

mkLinkIcon :: MonadWidget t m => (Int, Int) -> IconInfo -> m ()
mkLinkIcon (width, height) (IconInfo classPrefix iconSrc iconAlt href) =
  elAttr "a" ("href" =: href {-<> "class" =: "w-inline-block"-}) $ do
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
    , ("medium", "images/medium.svg", "Medium", "http://encoins-crypto.medium.com/")
    , ("email", "images/email.svg", "E-Mail", "")
    , ("telegram", "images/telegram.svg", "Telegram", "https://t.me/encoins_io")
    , ("docs", "images/docs.svg", "Source Docs", "https://encoins.gitbook.io/source-code-docs")
    , ("github", "images/github.svg", "GitHub", "https://github.com/encryptedcoins")
    , ("youtube", "images/youtube.svg", "YouTube", "")
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
        mapM_ mkLink navLinks
      divClass "flex-col-5" $ do
        elClass "span" "poppins-extra-bold-bianca-24px" $ text "Links"
        mapM_ mkLink intraLinks   
  icons :: MonadWidget t m => m ()
  icons = divClass "icons-small" $ do
    mapM_ (mkLinkIcon (30, 30)) iconInfos

  emailForm :: MonadWidget t m => m ()
  emailForm = elClass "form" "email-form" $ do
    evClick <- elAttr "a" ("class" =: "subscribe-button") $ button "Subscribe"
    iEl <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "placeholder" =: "E-mail"
               )
    pure ()