{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Reflex.Dom

import           Backend.Protocol.Types            (AesKeyRaw, PasswordRaw (..),
                                                    TokenCacheV3)
import           Backend.Status                    (AppStatus)
import           Backend.Utility                   (switchHoldDyn)
import           Backend.Wallet                    (Wallet (..))
import           ENCOINS.App.Widgets.Basic         (loadAppDataE)
import           ENCOINS.App.Widgets.IPFS          (checkTokens)
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
  -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dIpfsOn dmKey = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eNewTokensV3 <- switchHoldDyn dTab $ \tab -> mdo
      dOldTokensV3 :: Dynamic t [TokenCacheV3] <- loadAppDataE
        mPass encoinsV3 "mainWindow-key-encoinsV3" id []
      dOldTokensMigrated <- updateCacheV3 mPass dOldTokensV3

      dUpdatedStatusTokens <- checkTokens dIpfsOn dOldTokensMigrated

      dUpdatedTokensV3 <- case tab of
        WalletTab   -> walletTab mPass dWallet dUpdatedStatusTokens dIpfsOn dmKey
        TransferTab -> transferTab mPass dWallet dUpdatedStatusTokens dIpfsOn dmKey
        LedgerTab   -> ledgerTab mPass dUpdatedStatusTokens dIpfsOn dmKey
      return $ updated dUpdatedTokensV3
    holdDyn [] eNewTokensV3
