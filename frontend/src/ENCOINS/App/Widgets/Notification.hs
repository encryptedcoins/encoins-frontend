{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Notification where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

import Backend.Status
    ( AppStatus (..)
    , CloudIconStatus (..)
    , WalletStatus (..)
    , isAppTotalBlock
    , isAppStatusWantReload
    , isCloudIconStatus
    , textAppStatus
    , isTextAppStatus
    , isAppTxProcessingBlock
    )
import Backend.Utility (space, switchHoldDyn, toText)
import Backend.Wallet (Wallet (..))
import Config.Config (NetworkConfig (..), networkConfig)
import ENCOINS.App.Widgets.Basic (elementResultJS, singletonL)

import ENCOINS.Common.Events

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
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Event t [AppStatus]
    -> Event t AppStatus
    -> m (Dynamic t Text, Dynamic t Bool, Dynamic t CloudIconStatus, Dynamic t Bool)
handleAppStatus dWallet eAppStatusList eOtherTxStatus = do
    logEvent "AppStatus" $ fmap textAppStatus <$> eAppStatusList

    eCloudIconStatus <- getLastStatusE isCloudIconStatus eAppStatusList
    dWalletNetworkStatus <- fetchWalletNetworkStatus dWallet

    let eStatusNotification =
            leftmost
                [ eAppStatusList
                , singletonL . WalletInApp <$> updated dWalletNetworkStatus
                , singletonL <$> eOtherTxStatus
                ]

    dStatusText <-
        foldDynMaybe
            handleNotification
            (AppReady, T.empty)
            eStatusNotification

    let dIsBlockAllButtons = (isAppTotalBlock . fst) <$> dStatusText
    let dIsBlockConnectButton = (isAppTxProcessingBlock . fst) <$> dStatusText

    dCloudIconStatus <- holdDyn NoTokens eCloudIconStatus

    pure (snd <$> dStatusText, dIsBlockAllButtons, dCloudIconStatus, dIsBlockConnectButton)

handleNotification ::
    [AppStatus] -> (AppStatus, Text) -> Maybe (AppStatus, Text)
handleNotification appStatus previousStatus =
    -- Hold NoRelay status once it fired until page reloading.
    if isAppStatusWantReload (fst previousStatus)
        then Nothing
        else do
            s <- getLastStatus isTextAppStatus appStatus
            pure (s, textAppStatus s)

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
