module ENCOINS.App.Widgets.Navbar
    ( navbarWidget
    ) where

import Data.Bool (bool)
import Data.Text (Text)
import Reflex.Dom

import Backend.Protocol.Types (PasswordRaw)
import Backend.Status (CloudIconStatus (..))
import Backend.Utility (space)
import Backend.Wallet (Wallet (..), currentNetworkApp)
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Basic (logo)
import ENCOINS.Common.Widgets.Connect (connectWidget)
import ENCOINS.Common.Widgets.MoreMenu (NavMoreMenuClass (..), moreMenuWidget)

navbarWidget ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t Bool
    -> Maybe PasswordRaw
    -> Dynamic t Bool
    -> Dynamic t CloudIconStatus
    -> m (Event t (), Event t (), Event t (), Event t ())
navbarWidget w dIsBlock mPass dIsCloudOn dCloudStatus = do
    elAttr
        "div"
        ( "data-animation" =: "default"
            <> "data-collapse" =: "none"
            <> "data-duration" =: "400"
            <> "id" =: "Navbar"
            <> "data-easing" =: "ease"
            <> "data-easing2" =: "ease"
            <> "role" =: "banner"
            <> "class" =: "navbar w-nav"
        )
        $ divClass "navbar-container w-container"
        $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
                logo
                divClass "h3" $ text "ENCOINS"
            divClass "h4" $
                elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;") $
                    text currentNetworkApp
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                eConnect <- connectWidget w dIsBlock
                eCloud <- cloudIconWidget dIsCloudOn dIsBlock dCloudStatus
                eLocker <- lockerWidget mPass dIsBlock
                eMore <-
                    moreMenuWidget
                        (NavMoreMenuClass "common-Nav_Container_MoreMenu" "common-Nav_MoreMenu")
                pure (eLocker, eConnect, eCloud, eMore)

lockerWidget ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Dynamic t Bool
    -> m (Event t ())
lockerWidget mPass dIsBlock = do
    let iconClass = case mPass of
            Nothing -> "app-Nav_Locker-open"
            Just _ -> "app-Nav_Locker-close"
    let dLockerIconClass = bool iconClass (iconClass <> space <> "click-disabled") <$> dIsBlock
    elLocker <-
        divClass "menu-item app-Nav_LockerContainer" $
            fmap fst $
                elDynClass' "div" dLockerIconClass (pure ())
    pure $ domEvent Click elLocker

cloudIconWidget ::
    (MonadWidget t m) =>
    Dynamic t Bool
    -> Dynamic t Bool
    -> Dynamic t CloudIconStatus
    -> m (Event t ())
cloudIconWidget dIsCloudOn dIsBlock dCloudStatus = do
    let defaultClass = "menu-item app-Cloud_IconContainer"
    let dIconClass = zipDynWith selectIconClass dCloudStatus dIsCloudOn
    let dIconClassBlockable =
            zipDynWith
                (\isBlock cl -> bool cl (cl <> space <> "click-disabled") isBlock)
                dIsBlock
                dIconClass
    elCloud <-
        divClass defaultClass $
            fmap fst $
                elDynClass' "div" dIconClassBlockable (pure ())
    pure $ domEvent Click elCloud

selectIconClass :: CloudIconStatus -> Bool -> Text
selectIconClass status isOn = case (status, isOn) of
    (_, False) -> "app-Cloud_IconTurnOff"
    (NoTokens, _) -> "app-Cloud_IconTurnOff"
    (Saving, _) -> "app-Cloud_IconSaving"
    (AllSaved, _) -> "app-Cloud_IconAllSaved"
    (FailedSave, _) -> "app-Cloud_IconAttemptExcess"
