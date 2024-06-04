module ENCOINS.Website.Head
    ( headWidget
    ) where

import Reflex.Dom

import qualified JS.Website as JS

headWidget :: (MonadWidget t m) => m ()
headWidget = do
    meta $ "charset" =: "utf-8"
    el "title" $ text "ENCOINS"
    meta $
        "content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport"
    meta $ "content" =: "Webflow" <> "name" =: "generator"
    stylesheet "css/normalize.css"
    stylesheet "css/webflow.css"
    stylesheet "css/encoins.webflow.css"
    stylesheet "css/encoins.webflow-extra.css"
    stylesheet
        "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/css/all.min.css"

    elAttr
        "link"
        ( "href" =: "images/favicon.png"
            <> "rel" =: "shortcut icon"
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
    eWebpageLoaded <-
        domEvent Load . fst
            <$> elAttr' "script" ("src" =: "js/Webpage.js" <> "type" =: "text/javascript") blank

    -- Dex hunter begin
    eReactLoaded <-
        domEvent Load . fst
            <$> elAttr'
                "script"
                ( "src" =: "https://unpkg.com/react@18.2.0/umd/react.production.min.js"
                    <> "type" =: "text/javascript" <> "crossorigin" =: ""
                )
                blank
    eReactDomLoaded <-
        domEvent Load . fst
            <$> elAttr'
                "script"
                ( "src" =: "https://unpkg.com/react-dom@18.2.0/umd/react-dom.production.min.js"
                    <> "type" =: "text/javascript" <> "crossorigin" =: ""
                )
                blank
    eSwapsLoaded <-
        domEvent Load . fst
            <$> elAttr'
                "script"
                ( "src" =: "https://unpkg.com/@dexhunterio/swaps@0.0.100/lib/umd/swaps.umd.js"
                    <> "type" =: "module"
                )
                blank
    -- Dex hunter end


    dWebFontLoaded <- holdDyn False (True <$ eWebFontLoaded)
    dWebpageLoaded <- holdDyn False (True <$ eWebpageLoaded)

    -- Dex hunter begin
    dReactLoaded <- holdDyn False (True <$ eReactLoaded)
    dReactDomLoaded <- holdDyn False (True <$ eReactDomLoaded)
    dSwapsLoaded <- holdDyn False (True <$ eSwapsLoaded)
    -- Dex hunter end

    let eScriptsLoaded =
            ffilter (== True) $
                updated $
                    foldl
                        (zipDynWith (&&))
                        (pure True)
                        [ dWebFontLoaded
                        , dWebpageLoaded
                        , dReactLoaded
                        , dReactDomLoaded
                        , dSwapsLoaded
                        ]

    performEvent_ (JS.runHeadScripts <$ eScriptsLoaded)
    where
        meta attr = elAttr "meta" attr blank
        stylesheet href =
            elAttr
                "link"
                ("href" =: href <> "rel" =: "stylesheet" <> "type" =: "text/css")
                blank
