{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Website.Widgets.Navbar (navbarWidget) where

import           Control.Monad                (when)
import           Data.Bool                    (bool)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Reflex.Dom

import           ENCOINS.Common.Widgets.Basic (logo)

navbarWidget :: MonadWidget t m => Dynamic t (Text, Text) -> m (Event t (Text, Text))
navbarWidget dPageFocus = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "medium" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "index.html" <> "class" =: "brand w-nav-brand") logo
            divClass "menu-div-empty" blank
            divClass "menu-button w-nav-button" $
                divClass "w-icon-nav-menu" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ mdo
                let dPage = fmap fst dPageFocus
                eHome <- menuItemWidget "Home" "56.56" "#" False dPage
                eISPO <- menuItemWidget "ISPO" "43.77" "#" False dPage
                _     <- menuItemWidget "DAO" "118.45" "https://dao.encoins.io" False dPage
                _     <- menuItemWidget "White paper" "120.02" "docs/whitepaper.pdf" False dPage
                return $ (, "Navbar") <$> leftmost [eHome, eISPO]

menuItemWidget :: MonadWidget t m => Text -> Text -> Text -> Bool -> Dynamic t Text -> m (Event t Text)
menuItemWidget txt w ref isDisabled dTxt = divClass "menu-item-div " $ do
    let isSelected v   = txt == v
        clsSelected v  = bool "" "menu-item-selected " (isSelected v)
        clsDisabled    = bool "" "menu-item-disabled " isDisabled
        attrExternal   = bool mempty ("target" =: "_blank") (ref /= "#")
        mkAttrs v      = "href" =: ref <> "class" =: "menu-item w-nav-link " `Text.append` clsSelected v  `Text.append` clsDisabled
            <> attrExternal <> "style" =: "min-width: " `Text.append` w `Text.append` "px;"
        mkSelector v   = when (isSelected v) $ divClass "menu-selector" blank
    (e, _) <- elDynAttr' "a" (fmap mkAttrs dTxt) $ text txt
    dyn_ (fmap mkSelector dTxt)
    return $ txt <$ domEvent Click e
