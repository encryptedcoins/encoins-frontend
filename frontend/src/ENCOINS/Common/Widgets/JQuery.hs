module ENCOINS.Common.Widgets.JQuery where

import Data.Functor (($>))
import Reflex.Dom

jQueryWidget :: (MonadWidget t m) => m ()
jQueryWidget = do
    eJQueryLoaded <-
        domEvent Load . fst
            <$> elAttr'
                "script"
                ( "src"
                    =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
                    <> "type" =: "text/javascript"
                    <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0="
                    <> "crossorigin" =: "anonymous"
                )
                blank
    let e =
            eJQueryLoaded
                $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
    widgetHold_ blank e
