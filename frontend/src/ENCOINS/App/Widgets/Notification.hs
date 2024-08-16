{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Notification where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Reflex.Dom

import Backend.Status
    ( AppStatus (..)
    , CloudIconStatus (..)
    , WalletStatus (..)
    , isAppStatusWantReload
    , isAppTotalBlock
    , isAppTxProcessingBlock
    , isCloudIconStatus
    , isTextAppStatus
    , messageAppStatus
    )
import Backend.Wallet (Wallet (..))
import Common.Events
import Common.Reflex.Dom.Extra (elementResultJS)
import Common.Reflex.Extra (switchHoldDyn)
import Common.Utility (singletonL, space, toText)
import Config.Config (NetworkConfig (..), networkConfig)
import I18n.I18n (App)
import qualified I18n.Reflex.I18n as I18n
import qualified I18n.Common as I18n

fetchWalletNetworkStatus ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t WalletStatus)
fetchWalletNetworkStatus dWallet = do
    dWalletLoad <- elementResultJS "EndWalletLoad" id
    let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
    let eUnexpectedNetworkB =
            fmap
                (\w -> walletNetworkId w /= app networkConfig)
                eLoadedWallet
    let mkNetworkMessage isInvalidNetwork message =
            case (isInvalidNetwork, message) of
                (True, _) -> Just $ WalletNetworkError unexpectedNetworkApp
                (False, WalletReady) -> Nothing
                (False, _) -> Just WalletReady
    foldDynMaybe mkNetworkMessage WalletReady eUnexpectedNetworkB

unexpectedNetworkApp :: Text
unexpectedNetworkApp =
    "Unexpected network! Please switch the wallet to"
        <> space
        <> toText (app networkConfig)
        <> space
        <> "mode."

handleAppStatus ::
    (App t m) =>
    Dynamic t Wallet
    -> Event t [AppStatus]
    -> Event t AppStatus
    -> m (Dynamic t Text, Dynamic t Bool, Dynamic t CloudIconStatus, Dynamic t Bool)
handleAppStatus dWallet eAppStatusList eOtherTxStatus = do
    dWalletNetworkStatus <- fetchWalletNetworkStatus dWallet
    let eStatusNotification =
            leftmost
                [ eAppStatusList
                , singletonL . WalletInApp <$> updated dWalletNetworkStatus
                , singletonL <$> eOtherTxStatus
                ]

    eCloudIconStatus <- getLastStatusE isCloudIconStatus eAppStatusList

    dAppAndStatusMessage <-
        foldDynMaybe
            handleNotification
            AppReady
            eStatusNotification

    let dIsBlockAllButtons = isAppTotalBlock <$> dAppAndStatusMessage
    let dIsBlockConnectButton = isAppTxProcessingBlock <$> dAppAndStatusMessage

    dCloudIconStatus <- holdDyn NoTokens eCloudIconStatus

    dLocale <- I18n.askLocale
    let localizer ::
            I18n.Locale
            -> Either I18n.StatusMessage (I18n.StatusMessage, I18n.StatusMessage)
            -> Text
        localizer l = \case
            Left m -> I18n.localizeWith l m
            Right (m1, m2) -> I18n.localizeWith l m1 <> I18n.localizeWith l m2
    let dStatusMessage = messageAppStatus <$> dAppAndStatusMessage
    let dStatusText = zipDynWith localizer dLocale dStatusMessage
    logDyn "AppStatus" (localizer I18n.Locale_EN <$> dStatusMessage)

    pure
        ( dStatusText
        , dIsBlockAllButtons
        , dCloudIconStatus
        , dIsBlockConnectButton
        )

handleNotification ::
    [AppStatus]
    -> AppStatus
    -> Maybe AppStatus
handleNotification appStatusList previousStatus =
    -- Hold NoRelay status once it fired until page reloading.
    if isAppStatusWantReload previousStatus
        then Nothing
        else getLastStatus isTextAppStatus appStatusList

getLastStatus :: (a -> Maybe b) -> [a] -> Maybe b
getLastStatus f = fmap NE.last . NE.nonEmpty . mapMaybe f

getLastStatusE ::
    (MonadWidget t m) => (a -> Maybe b) -> Event t [a] -> m (Event t b)
getLastStatusE f eList = do
    let emLastStatus = getLastStatus f <$> eList
    dmLastStatus <- holdDyn Nothing emLastStatus
    switchHoldDyn dmLastStatus $ \case
        Nothing -> pure never
        Just s -> do
            ev <- newEvent
            pure $ s <$ ev
