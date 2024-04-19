module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Bool                     (bool)
import           Data.Text                     (Text, take, takeEnd)
import           Prelude                       hiding (take)
import           Reflex.Dom

import           Backend.Protocol.Types        (PasswordRaw)
import           Backend.Status                (CloudStatusIcon (..))
import           Backend.Utility               (space)
import           Backend.Wallet                (Wallet (..), WalletName (..),
                                                currentNetworkApp)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Basic  (btnWithBlock, logo)
import           ENCOINS.Common.Widgets.Wallet (walletIcon)

connectText :: Wallet -> Text
connectText w = case w of
  Wallet None _ _    _ _ -> "CONNECT"
  Wallet _    _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

navbarWidget :: MonadWidget t m
  => Dynamic t Wallet
  -> Dynamic t Bool
  -> Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t CloudStatusIcon
  -> m (Event t (), Event t (), Event t (), Event t ())
navbarWidget w dIsBlock mPass dIsCloudOn dCloudStatus= do
  elAttr "div" ("data-animation" =: "default" <> "data-collapse" =: "none" <> "data-duration" =: "400" <> "id" =: "Navbar"
    <> "data-easing" =: "ease" <> "data-easing2" =: "ease" <> "role" =: "banner" <> "class" =: "navbar w-nav") $
    divClass "navbar-container w-container" $ do
            elAttr "a" ("href" =: "https://encoins.io" <> "class" =: "brand w-nav-brand") do
              logo
              divClass "h3" $ text "ENCOINS"
            divClass "h4"
              $ elAttr "div" ("style" =: "font-size: 20px; margin-left: 10px;")
              $ text currentNetworkApp
            divClass "menu-div-empty" blank
            elAttr "nav" ("role" =: "navigation" <> "class" =: "nav-menu w-nav-menu") $ do
                eConnect <- divClass "menu-item-button-left" $
                    btnWithBlock "button-switching flex-center" "" dIsBlock $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                elCloud <- cloudIconWidget dIsCloudOn dIsBlock dCloudStatus
                elLocker <- lockerWidget mPass dIsBlock
                elMore <- moreMenuWidget
                return (domEvent Click elLocker, eConnect, domEvent Click elCloud, domEvent Click elMore)

moreMenuWidget :: MonadWidget  t m
  => m (Element EventResult (DomBuilderSpace m) t)
moreMenuWidget = do
  divClass "menu-item app-Nav_Container_MoreMenu" $
    fmap fst $ elDynClass' "div" (constDyn "app-Nav_MoreMenu") (pure ())

lockerWidget :: MonadWidget  t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerWidget mPass dIsBlock = do
  let iconClass = case mPass of
        Nothing -> "app-Nav_Locker-open"
        Just _  -> "app-Nav_Locker-close"
  let dLockerIconClass = bool iconClass (iconClass <> space <> "click-disabled") <$> dIsBlock
  divClass "menu-item app-Nav_LockerContainer" $
    fmap fst $ elDynClass' "div" dLockerIconClass (pure ())

cloudIconWidget :: MonadWidget  t m
  => Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t CloudStatusIcon
  -> m (Element EventResult (DomBuilderSpace m) t)
cloudIconWidget dIsCloudOn dIsBlock dCloudStatus = do
  let defaultClass = "menu-item app-Cloud_IconContainer"
  let dIconClass = zipDynWith selectIconClass dCloudStatus dIsCloudOn
  let dIconClassBlockable = zipDynWith
        (\isBlock cl -> bool cl (cl <> space <> "click-disabled") isBlock )
        dIsBlock
        dIconClass
  divClass defaultClass $ do
    fmap fst $ elDynClass' "div" dIconClassBlockable (pure ())

selectIconClass :: CloudStatusIcon -> Bool -> Text
selectIconClass status isOn = case (status, isOn) of
  (_, False)      -> "app-Cloud_IconTurnOff"
  (NoTokens, _)   -> "app-Cloud_IconTurnOff"
  (Saving, _)     -> "app-Cloud_IconSaving"
  (AllSaved, _)   -> "app-Cloud_IconAllSaved"
  (FailedSave, _) -> "app-Cloud_IconAttemptExcess"
