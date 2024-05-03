module ENCOINS.Website.Widgets.Footer
    ( footerWidget
    ) where

import Data.Text (Text, pack)
import Data.Version (showVersion)
import Paths_encoins_frontend (version)
import Reflex.Dom

import ENCOINS.Common.Widgets.Basic
import ENCOINS.Website.Widgets.Resourses (ourResourses)

footerWidget :: (MonadWidget t m) => m (Event t (Text, Text))
footerWidget = divClass "footer wf-section" $ do
    res <- divClass "container-footer" $
        divClass "columns-footer w-row" $ do
            divClass "column-footer border-right w-col w-col-6" $ do
                divClass "div-horizontal-margin" $ divClass "div-horizontal-items" $ do
                    logo
                    h3 "ENCOINS"
                divClass "div-horizontal-margin div-vertical-margin" $ pClass "p-footer" $ do
                    text
                        "ENCOINS is a decentralized private accounts and payments protocol on the Cardano blockchain. "
                    text "ENCS utility token policyID: "
                    lnkInline
                        "https://coinmarketcap.com/currencies/encoins/"
                        "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96"
                    text "."
                divClass "div-horizontal-margin" $ do
                    h5 "Our Resourses"
                    divClass "div-our-resourses div-justify-left" $ ourResourses "50px"
                divClass "div-vertical-margin div-horizontal-margin" $
                    pClass "p-footer p-invisible" $
                        text "All rights reserved@2022"
            divClass "column-footer w-col w-col-6" $ do
                divClass "div-vertical-margin div-horizontal-margin div-invisible" $
                    h5 "Subscribe for updates"
                divClass "div-vertical-margin div-horizontal-margin div-invisible" $ divClass "form-email-submit" $ do
                    divClass "text-input" $ text "Email Address"
                    _ <- btn "" "" $ text "Submit"
                    blank
                divClass "div-vertical-margin div-horizontal-margin div-invisible" blank
                divClass "div-horizontal-margin" $ divClass "columns-footer w-row" $ do
                    e <- divClass "column-footer w-col w-col-6" $ do
                        h5Bold "About"
                        eFeatures <- lnk "#Features" "" $ divClass "text-footer" $ text "Features"
                        eRoadmap <- lnk "#Roadmap" "" $ divClass "text-footer" $ text "Roadmap"
                        _ <-
                            lnk "#Partners" "" $ divClass "text-footer text-invisible" $ text "Partners"
                        return $
                            leftmost [("Home", "Features") <$ eFeatures, ("Home", "Roadmap") <$ eRoadmap]
                    divClass "column-footer w-col w-col-6" $ do
                        h5Bold "Links"
                        eHome <- lnk "#Navbar" "" $ divClass "text-footer" $ text "Home"
                        eISPO <- lnk "#Navbar" "" $ divClass "text-footer" $ text "ISPO"
                        _ <- lnk "https://dao.encoins.io" "" $ divClass "text-footer" $ text "DAO"
                        _ <-
                            lnk "docs/whitepaper.pdf" "" $ divClass "text-footer" $ text "White Paper"
                        return $ leftmost [("Home", "Navbar") <$ eHome, ("ISPO", "Navbar") <$ eISPO, e]
    versionWidget
    pure res

versionWidget :: (MonadWidget t m) => m ()
versionWidget =
    divClass "main-Version" $
        divClass "main-Footer_VersionContainer" $
            divClass "main-Footer_VersionText" $
                text $
                    "Encoins frontend v" <> pack (showVersion version)
