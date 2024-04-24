{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import Data.Maybe (isNothing)
import Reflex.Dom

import Backend.Protocol.Types
import Backend.Status (AppStatus)
import Backend.Utility (switchHoldDyn)
import Backend.Wallet (Wallet (..))
import ENCOINS.App.Widgets.Basic (loadAppDataME)
import ENCOINS.App.Widgets.Cloud
    ( resetTokens
    , restoreValidTokens
    , syncRestoreWithCache
    )
import ENCOINS.App.Widgets.MainTabs (ledgerTab, transferTab, walletTab)
import ENCOINS.App.Widgets.Migration (migrateTokenCacheV3)
import ENCOINS.App.Widgets.TabsSelection (AppTab (..), tabsSection)
import ENCOINS.Common.Cache (encoinsV3)
import ENCOINS.Common.Events

mainWindow ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Dynamic t Wallet
    -> Dynamic t Bool
    -> Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Dynamic t Bool
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dCloudOn dmKey dKeyWasReset eRestore = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eNewTokensV3 <- switchHoldDyn dTab $ \tab -> mdo
        dmOldTokensV3 :: Dynamic t (Maybe [TokenCacheV3]) <-
            loadAppDataME
                mPass
                encoinsV3
                "mainWindow-key-encoinsV3"
        -- logDyn "mainWindow: dmOldTokensV3" $ fmap showTokens <$> dmOldTokensV3

        -- Reset tokens to SaveUndefined status when aes key changed
        dKeyWasResetFired <-
            holdDyn False $ tagPromptlyDyn dKeyWasReset $ updated dKeyWasReset
        let dmOldTokensStatusUpdated = resetTokens dmOldTokensV3 dKeyWasResetFired

        -- Migrate from old token structure to version 3
        -- if tokensV3 are not found in cache
        dOldTokensMigratedUniq <-
            holdUniqDyn =<< migrateTokenCacheV3 mPass dmOldTokensStatusUpdated
        -- logDyn "mainWindow: dOldTokensMigratedUniq" $ showTokens <$> dOldTokensMigratedUniq

        let eWasMigration =
                ()
                    <$ ( ffilter isNothing $
                            tagPromptlyDyn dmOldTokensStatusUpdated $
                                updated dOldTokensMigratedUniq
                       )
        -- logEvent "mainWindow: eWasMigration" eWasMigration

        -- Restore tokens from remote server and sync it with local cache
        dRestoreResponse <- restoreValidTokens dmKey eRestore
        -- logDyn "mainWindow: dRestoreResponse" $ showTokens <$> dRestoreResponse

        let dTokens = zipDynWith syncRestoreWithCache dRestoreResponse dOldTokensMigratedUniq
        -- logDyn "mainWindow: dTokens" $ showTokens <$> dTokens

        dUpdatedTokensV3 <- case tab of
            WalletTab -> walletTab mPass dWallet dTokens dCloudOn dmKey eWasMigration
            TransferTab -> transferTab mPass dWallet dTokens dCloudOn dmKey eWasMigration
            LedgerTab -> ledgerTab mPass dTokens dCloudOn dmKey eWasMigration
        return $ updated dUpdatedTokensV3
    holdDyn [] eNewTokensV3
