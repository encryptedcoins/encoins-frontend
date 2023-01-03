module ENCOINS.Website.Widgets.Navbar (navbarWidget) where

import           Control.Monad          (when)
import           Data.Bool              (bool)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Reflex.Dom

navbarWidget :: MonadWidget t m => m (Dynamic t Text)
navbarWidget = do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "medium" <> "data-duration" =: "400"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "div-navbar" $
        divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "index.html" <> "aria-current" =: "page" <> "class" =: "brand w-nav-brand w--current") $
                elAttr "img" ("src" =: "images/logo.svg" <> "loading" =: "lazy" <> "alt" =: "" <> "class" =: "logo") blank
            divClass "menu-div-empty" blank
            divClass "menu-button w-nav-button" $
                divClass "w-icon-nav-menu" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ mdo
                eHome <- menuItemWidget "Home" "56.56" "#" dMenuItem
                eISPO <- menuItemWidget "ISPO" "43.77" "#" dMenuItem
                eGov  <- menuItemWidget "Governance" "118.45" "#" dMenuItem
                _     <- menuItemWidget "White paper" "114.38" "docs/whitepaper.pdf" dMenuItem
                dMenuItem <- holdDyn  "Home" $ leftmost [eHome, eISPO, eGov]
                return dMenuItem

menuItemWidget :: MonadWidget t m => Text -> Text -> Text -> Dynamic t Text -> m (Event t Text)
menuItemWidget txt w lnk dTxt = divClass "menu-item-div " $ do
    let isSelected v   = txt == v
        clsSelected v  = bool "" "menu-item-selected" (isSelected v)
        attrExternal   = bool mempty ("target" =: "_blank") (lnk /= "#")
        mkAttrs v      = "href" =: lnk <> "class" =: "menu-item w-nav-link " `Text.append` clsSelected v <> attrExternal
            <> "style" =: "width: " `Text.append` w `Text.append` "px;"
        mkSelector v   = when (isSelected v) $ divClass "menu-selector" blank
    (e, _) <- elDynAttr' "a" (fmap mkAttrs dTxt) $ text txt
    dyn_ (fmap mkSelector dTxt)
    return $ txt <$ domEvent Click e