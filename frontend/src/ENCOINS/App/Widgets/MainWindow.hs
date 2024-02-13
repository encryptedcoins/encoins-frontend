{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Text                         (Text)
import           Reflex.Dom

import           Backend.Protocol.Types            (PasswordRaw (..),
                                                    TokenCacheV3, AesKeyRaw)
import           Backend.Status                    (Status (..))
import           Backend.Utility                   (switchHoldDyn)
import           Backend.Wallet                    (Wallet (..))
import           ENCOINS.App.Widgets.Basic         (loadAppData)
import           ENCOINS.App.Widgets.MainTabs      (ledgerTab, transferTab,
                                                    walletTab)
import           ENCOINS.App.Widgets.Migration     (updateCacheV3)
import           ENCOINS.App.Widgets.TabsSelection (AppTab (..), tabsSection)
import           ENCOINS.Common.Cache              (encoinsV3)
import           ENCOINS.Common.Events

mainWindow :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dIpfsOn dmKey = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eSecrets <- switchHoldDyn dTab $ \tab -> mdo
      dSecretsV3 :: Dynamic t [TokenCacheV3] <- loadAppData mPass encoinsV3 id []
      updateCacheV3 mPass dSecretsV3

      case tab of
        WalletTab   -> walletTab mPass dWallet dSecretsV3 dIpfsOn dmKey
        TransferTab -> transferTab mPass dWallet dSecretsV3
        LedgerTab   -> ledgerTab mPass dSecretsV3 dIpfsOn dmKey
      return $ updated dSecretsV3
    holdDyn [] eSecrets
