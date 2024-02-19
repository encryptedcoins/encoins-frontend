module ENCOINS.App.Widgets.Navbar (navbarWidget) where

import           Data.Bool                     (bool)
import           Data.Map                      (Map)
import           Data.Text                     (Text, take, takeEnd)
import           Prelude                       hiding (take)
import           Reflex.Dom

import           Backend.Protocol.Types        (PasswordRaw)
import           Backend.Status                (IpfsSaveStatus (..))
import           Backend.Utility               (space)
import           Backend.Wallet                (Wallet (..), WalletName (..),
                                                currentNetworkApp)
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
  -> Dynamic t IpfsSaveStatus
  -> m (Event t (), Event t (), Event t ())
navbarWidget w dIsBlock mPass dIsIpfsOn dIpfsStatus= do
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
                elLocker <- lockerWidget mPass dIsBlock
                elIpfs <- ipfsIconWidget dIsIpfsOn dIsBlock dIpfsStatus
                eConnect <- divClass "menu-item-button-left" $
                    btnWithBlock "button-switching flex-center" "" dIsBlock $ do
                        dyn_ $ fmap (walletIcon . walletName) w
                        dynText $ fmap connectText w
                return (domEvent Click elLocker, eConnect, domEvent Click elIpfs)

lockerWidget :: MonadWidget  t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerWidget mPass dIsBlock = do
  let (iconClass, popupText) = case mPass of
        Nothing -> ("app-Nav_Locker-open", "isn't protected")
        Just _  -> ("app-Nav_Locker-close", "is protected")
  let defaultClass = "menu-item app-Nav_LockerContainer" <> space <> iconClass
  let dClass = bool defaultClass (defaultClass <> space <> "click-disabled") <$> dIsBlock
  let dClassMap = (\cl -> "class" =: cl) <$> dClass
  lockerDiv dClassMap popupText

lockerDiv :: MonadWidget t m
  => Dynamic t (Map Text Text)
  -> Text
  -> m (Element EventResult (DomBuilderSpace m) t)
lockerDiv dClassMap popupText
  = fmap fst $ elDynAttr' "div" dClassMap
      $ divClass "app-Nav_CachePopup"
      $ el "p" $ text $ "Cache" <> space <> popupText

ipfsIconWidget :: MonadWidget  t m
  => Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t IpfsSaveStatus
  -> m (Element EventResult (DomBuilderSpace m) t)
ipfsIconWidget dIsIpfsOn dIsBlock dIpfsStatus = do
  let dPopup = bool
        "IPFS sync is on"
        "IPFS sync is off"
        <$> dIsIpfsOn
  let defaultClass = "menu-item app-Ipfs_IconContainer"
  let dIconClass = (\iCl -> defaultClass <> space <> iCl) . selectIconClass <$> dIpfsStatus
  let dClass = zipDynWith
        (\isBlock cl -> bool cl (cl <> space <> "click-disabled") isBlock )
        dIsBlock
        dIconClass
  let dClassMap = (\cl -> "class" =: cl) <$> dClass
  ipfsPopup dClassMap dPopup

ipfsPopup :: MonadWidget t m
  => Dynamic t (Map Text Text)
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
ipfsPopup dClassMap dPopup
  = fmap fst $ elDynAttr' "div" dClassMap
      $ divClass "app-Nav_IpfsPopup"
      $ el "p" $ dynText dPopup

selectIconClass :: IpfsSaveStatus -> Text
selectIconClass = \case
  TurnOff       -> "app_Ipfs_IconTurnOff"
  Pinning       -> "app_Ipfs_IconPinning"
  PinnedAll     -> "app_Ipfs_IconPinnedAll"
  AttemptExcess -> "app_Ipfs_IconAttemptExcess"
