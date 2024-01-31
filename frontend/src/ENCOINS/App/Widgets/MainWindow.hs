{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Text                         (Text)
import           Reflex.Dom

import           Backend.Protocol.Types            (PasswordRaw (..),
                                                    TokenCacheV3)
import           Backend.Status                    (Status (..))
import           Backend.Utility                   (switchHoldDyn)
import           Backend.Wallet                    (Wallet (..))
import           ENCOINS.App.Widgets.Basic         (loadAppData, saveAppDataId_)
import           ENCOINS.App.Widgets.IPFS          (pinEncryptedTokens)
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
  -> Dynamic t (Maybe Text)
  -> Dynamic t Bool
  -> m (Dynamic t [TokenCacheV3])
mainWindow mPass dWallet dIsDisableButtons dmKey dIpfsOn = mdo
    eTab <- tabsSection dTab dIsDisableButtons
    dTab <- holdDyn WalletTab eTab
    eSecrets <- switchHoldDyn dTab $ \tab -> mdo
      dSecretsV3 :: Dynamic t [TokenCacheV3] <- loadAppData mPass encoinsV3 id []
      logDyn "mainWindow: dSecretsV3" dSecretsV3
      updateCacheV3 mPass dSecretsV3

      -- eTokenIpfsCached <- pinEncryptedTokens
      --   (walletAddressBech32 <$> dWallet)
      --   mPass
      --   dmKey
      --   dIpfsOn
      --   (dSecretsV3)
      -- logEvent "mainWindow: eTokenIpfsCached" eTokenIpfsCached
      -- saveAppDataId_ mPass encoinsV3 eTokenIpfsCached

      case tab of
        WalletTab   -> walletTab mPass dWallet dSecretsV3
        TransferTab -> transferTab mPass dWallet dSecretsV3
        LedgerTab   -> ledgerTab mPass dSecretsV3
      return $ updated dSecretsV3
    -- logEvent "mainWindow: eSecrets" eSecrets
    holdDyn [] eSecrets
