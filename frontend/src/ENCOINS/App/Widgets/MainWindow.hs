{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Maybe                        (isNothing)
import           Reflex.Dom

import           Backend.Protocol.Types
import           Backend.Status                    (AppStatus)
import           Backend.Utility                   (switchHoldDyn)
import           Backend.Wallet                    (Wallet (..))
import           ENCOINS.App.Widgets.Basic         (loadAppDataME)
import           ENCOINS.App.Widgets.MainTabs      (ledgerTab, transferTab,
                                                    walletTab)
import           ENCOINS.App.Widgets.Migration     (updateCacheV3)
import           ENCOINS.App.Widgets.Save          (resetTokens)
import           ENCOINS.App.Widgets.TabsSelection (AppTab (..), tabsSection)
import           ENCOINS.Common.Cache              (encoinsV3)
import           ENCOINS.Common.Events
import ENCOINS.App.Widgets.Save (restoreValidTokens)


mainWindow :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t Bool
  -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dSaveOn dmKey dKeyWasReset = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eNewTokensV3 <- switchHoldDyn dTab $ \tab -> mdo
      dmOldTokensV3 :: Dynamic t (Maybe [TokenCacheV3]) <- loadAppDataME
          mPass encoinsV3 "mainWindow-key-encoinsV3"

      -- Reset tokens to SaveUndefined status when aes key changed
      dKeyWasResetFired <- holdDyn False $ tagPromptlyDyn dKeyWasReset $ updated dKeyWasReset
      let dmOldTokensStatusUpdated = resetTokens dmOldTokensV3 dKeyWasResetFired

      dRestoreResponse <- restoreValidTokens dmKey
      -- logDyn "mainWindow: dRestoreResponse" $ length <$> dRestoreResponse

      -- Migrate from old token structure to version 3
      -- if tokensV3 are not found in cache
      dOldTokensMigratedUniq <- holdUniqDyn =<< updateCacheV3 mPass dmOldTokensStatusUpdated
      -- logDyn "mainWindow: dOldTokensMigratedUniq" $ showTokens <$> dOldTokensMigratedUniq

      let eWasMigration = () <$ (ffilter isNothing
            $ tagPromptlyDyn dmOldTokensStatusUpdated
            $ updated dOldTokensMigratedUniq)
      -- logEvent "mainWindow: eWasMigration" eWasMigration

      dUpdatedTokensV3 <- case tab of
        WalletTab   -> walletTab mPass dWallet dOldTokensMigratedUniq dSaveOn dmKey eWasMigration
        TransferTab -> transferTab mPass dWallet dOldTokensMigratedUniq dSaveOn dmKey eWasMigration
        LedgerTab   -> ledgerTab mPass dOldTokensMigratedUniq dSaveOn dmKey eWasMigration
      return $ updated dUpdatedTokensV3
    holdDyn [] eNewTokensV3
