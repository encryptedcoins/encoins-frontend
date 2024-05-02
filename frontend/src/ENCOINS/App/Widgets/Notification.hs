{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Notification where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

import Backend.Status
    ( AppStatus
    , CloudStatusIcon (..)
    , Status (..)
    , isReady
    , isSaveStatus
    , isStatus
    , isStatusWantBlockButtons
    , isStatusWantReload
    )
import Backend.Utility (column, space, switchHoldDyn, toText)
import Backend.Wallet (Wallet (..))
import Config.Config (NetworkConfig (..), networkConfig)
import ENCOINS.App.Widgets.Basic (elementResultJS)

import ENCOINS.Common.Events

fetchWalletNetworkStatus ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> m (Dynamic t (Text, Status))
fetchWalletNetworkStatus dWallet = do
    dWalletLoad <- elementResultJS "EndWalletLoad" id
    let eLoadedWallet = tagPromptlyDyn dWallet $ updated dWalletLoad
    let eUnexpectedNetworkB =
            fmap
                (\w -> walletNetworkId w /= app networkConfig)
                eLoadedWallet
    let mkNetworkMessage isInvalidNetwork message =
            case (isInvalidNetwork, message) of
                (True, _) -> Just ("NetworkId status", WalletNetworkError unexpectedNetworkApp)
                (False, ("", Ready)) -> Nothing
                (False, _) -> Just (T.empty, Ready)
    foldDynMaybe mkNetworkMessage (T.empty, Ready) eUnexpectedNetworkB

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
    -> Event t (Text, Status)
    -> m (Dynamic t Text, Dynamic t Bool, Dynamic t CloudStatusIcon)
handleAppStatus dWallet elAppStatus eAppStatus = do
    logEvent "AppStatus" elAppStatus
    eSaveStatus <- getLastStatus isSaveStatus elAppStatus
    eStatus <- getLastStatus isStatus elAppStatus
    dWalletNetworkStatus <- fetchWalletNetworkStatus dWallet

    dStatus <-
        foldDynMaybe
            -- Hold NoRelay status once it fired until page reloading.
            (\ev (_, accS) -> if isStatusWantReload accS then Nothing else Just ev)
            (T.empty, Ready)
            $ leftmost [eStatus, updated dWalletNetworkStatus, eAppStatus]

    let flatStatus (t, s)
            | isReady s = T.empty
            | T.null $ T.strip t = toText s
            | otherwise = t <> column <> space <> toText s

    let dIsDisableButtons = (isStatusWantBlockButtons . snd) <$> dStatus
    let dStatusT = flatStatus <$> dStatus

    dCloudStatus <- holdDyn NoTokens eSaveStatus

    pure (dStatusT, dIsDisableButtons, dCloudStatus)

getLastStatus ::
    (MonadWidget t m) => (a -> Maybe b) -> Event t [a] -> m (Event t b)
getLastStatus f eList = do
    let emLastStatus = fmap NE.last . NE.nonEmpty . mapMaybe f <$> eList
    dmLastStatus <- holdDyn Nothing emLastStatus
    switchHoldDyn dmLastStatus $ \case
        Nothing -> pure never
        Just s -> do
            ev <- newEvent
            pure $ s <$ ev
