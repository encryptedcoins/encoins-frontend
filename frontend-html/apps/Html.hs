{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base

main :: IO ()
main = do
    renderToFile "result/index.html" indexHtml
    renderToFile "result/app.html" appHtml
    renderToFile "result/dao.html" daoHtml

indexHtml :: Html ()
indexHtml = makePage "63b058a2f897ba88fad5ff1c" "index"

appHtml :: Html ()
appHtml = makePage "63c13412f02e2d900176a1bc" "app"

daoHtml :: Html ()
daoHtml = makePage "63b058a2f897ba88fad5ff1c" "dao"

makePage :: Text -> Text -> Html ()
makePage pageValue file = do
    doctype_
    html_
        [ mkDataWfPage pageValue
        , dataWfSite
        ]
        $ do
            head_ $ do
                title_ "Encoins"
                noscript_ $
                    link_ [rel_ "stylesheet", href_ "css/noscript.css"]
            body_ $ do
                script_ [src_ $ file <> ".js"] T.empty
                noScript

mkDataWfPage :: Text -> Attributes
mkDataWfPage = makeAttributes "data-wf-page"

dataWfSite :: Attributes
dataWfSite = makeAttributes "data-wf-site" "63b058a2f897ba2767d5ff1b"

noScript :: Html ()
noScript = noscript_ $
    div_ [class_ "no-script"] $ do
        img_
            [ class_ "javascript javascript_inverted"
            , src_ "images/javascript.svg"
            ]
        p_
            [ class_ "text"
            ]
            "JavaScript is disabled"
