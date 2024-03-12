{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Reflex.Dom

import           Backend.Protocol.Types            (AesKeyRaw, PasswordRaw (..),
                                                    TokenCacheV3 (..))
import           Backend.Status                    (AppStatus)
import           Backend.Utility                   (switchHoldDyn)
import           Backend.Wallet                    (Wallet (..))
import           ENCOINS.App.Widgets.Basic         (loadAppDataME)
import           ENCOINS.App.Widgets.IPFS          (resetTokens)
import           ENCOINS.App.Widgets.MainTabs      (ledgerTab, transferTab,
                                                    walletTab)
import           ENCOINS.App.Widgets.Migration     (updateCacheV3)
import           ENCOINS.App.Widgets.TabsSelection (AppTab (..), tabsSection)
import           ENCOINS.Common.Cache              (encoinsV3)
import           ENCOINS.Common.Events

mainWindow :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t Bool
  -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dIpfsOn dmKey dKeyTheSame = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eNewTokensV3 <- switchHoldDyn dTab $ \tab -> mdo
      dmOldTokensV3 :: Dynamic t (Maybe [TokenCacheV3]) <- loadAppDataME
          mPass encoinsV3 "mainWindow-key-encoinsV3"

      -- Reset tokens to IpfsUndefined status when aes key changed
      let dmOldTokensV3StatusUpdated = resetTokens dmOldTokensV3 dKeyTheSame

      -- Migrate from old token structure to version 3
      -- if tokensV3 are not found in cache
      dOldTokensMigrated <- updateCacheV3 mPass dmOldTokensV3StatusUpdated

      dUpdatedTokensV3 <- case tab of
        WalletTab   -> walletTab mPass dWallet dOldTokensMigrated dIpfsOn dmKey
        TransferTab -> transferTab mPass dWallet dOldTokensMigrated dIpfsOn dmKey
        LedgerTab   -> ledgerTab mPass dOldTokensMigrated dIpfsOn dmKey
      return $ updated dUpdatedTokensV3
    holdDyn [] eNewTokensV3
