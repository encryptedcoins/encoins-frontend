module ENCOINS.App.Widgets.MainTabs where

import           Control.Monad                          (void)
import           Data.Aeson                             (encode)
import           Data.Bool                              (bool)
import           Data.ByteString.Lazy                   (toStrict)
import           Data.List                              (nub)
import           Data.Text                              (Text)
import           Data.Text.Encoding                     (decodeUtf8)
import           Reflex.Dom
import           Witherable                             (catMaybes)

import           Backend.EncoinsTx                      (encoinsTxWalletMode, encoinsTxTransferMode, encoinsTxLedgerMode)
import           Backend.Environment                    (getEnvironment)
import           Backend.Protocol.Fees                  (protocolFees)
import           Backend.Protocol.Types
import           Backend.Status                         (Status(..), walletError, isStatusBusy)
import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp, elementResultJS)
import           ENCOINS.App.Widgets.Coin               (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                          coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget, coinNewButtonWidget)
import           ENCOINS.App.Widgets.InputAddressWindow (inputAddressWindow)
import           ENCOINS.App.Widgets.ImportWindow       (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.SendRequestButton  (sendRequestButton)
import           ENCOINS.App.Widgets.SendToWalletWindow (sendToWalletWindow)
import           ENCOINS.App.Widgets.TransactionBalance (transactionBalanceWidget)
import           ENCOINS.App.Widgets.WelcomeWindow      (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey, welcomeLedger, welcomeWindowLedgerStorageKey)
import           ENCOINS.Bulletproofs                   (Secrets)
import           ENCOINS.Common.Utils                   (toText)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           JS.Website                             (saveJSON)

mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

walletTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet -> Dynamic t Secrets -> m ()
walletTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    (dBalance, dFees, dBulletproofParams, bRandomness) <- getEnvironment WalletMode dWallet (pure Nothing) dToBurn dToMint

    containerApp "" $ transactionBalanceWidget dBalance dFees
    (dToBurn, dToMint, eStatusUpdate) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets $ zipDynWith (++) dOldSecrets dNewSecrets
            dSecretsWithNames <- coinCollectionWithNames dSecrets
            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 . toStrict . encode <$> updated dSecrets)

            (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-wallet-coins" $ do
                  mainWindowColumnHeader "Coins in the Wallet"
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                  coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                eImp <- divClassId "" "welcome-import-export" $ do
                    (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
                    (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
                    exportWindow eExport dCTB
                    exportWindow eExportAll dSecrets
                    eIS    <- fmap pure . catMaybes <$> importWindow eImport
                    eISAll <- importFileWindow eImportAll
                    return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                dCoinsToMint' <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret <- coinNewWidget
                    return dCoinsToMint''
                eSend' <- sendRequestButton WalletMode dStatus dWallet dCoinsToBurn dCoinsToMint (void $ updated dBalance)
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate, _) <- encoinsTxWalletMode dWallet dBulletproofParams bRandomness dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClassId "app-text-small" "welcome-read-docs" $ dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text

transferTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet -> Dynamic t Secrets -> m ()
transferTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer

    containerApp "" $ transactionBalanceWidget (pure 0) (pure 0)
    (dCoins, eSendToLedger, eAddr, dSecretsWithNames) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets dOldSecrets
        dSecretsWithNames <- coinCollectionWithNames dSecrets
        performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 . toStrict . encode <$> updated dSecrets)

        (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
            dCTB <- divClassId "" "welcome-coins-transfer" $ do
                mainWindowColumnHeader "Coins in the Wallet"
                dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                coinBurnCollectionWidget dSecretsWithNamesInTheWallet
            (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
            (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
            exportWindow eExport dCTB
            exportWindow eExportAll dSecrets
            eIS    <- fmap pure . catMaybes <$> importWindow eImport
            eISAll <- importFileWindow eImportAll
            return (dCTB, leftmost [eIS, eISAll])
        divClassId "app-column w-col w-col-6" "welcome-transfer-btns" $ do
          eWallet      <- sendButton (zipDynWith (&&) (fmap (not . null) dCoinsToBurn) (fmap (not . isStatusBusy) dStatus)) "" " Send to a Wallet"
          eLedger      <- sendButton (zipDynWith (&&) (fmap (not . null) dCoinsToBurn) (fmap (not . isStatusBusy) dStatus)) "margin-top: 20px" " Send to the Ledger"
          eWalletOk    <- sendToWalletWindow eWallet dCoinsToBurn
          (eAddrOk, _) <- inputAddressWindow eWalletOk
          return (dCoinsToBurn, eLedger, eAddrOk, dSecretsWithNames)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    (dAssetNamesInTheWallet, eStatusUpdate1, _) <- encoinsTxTransferMode dWallet dCoins dSecretsWithNamesInTheWallet dAddr (void eAddr) dWalletSignature
    let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
    (_, eStatusUpdate2, _) <- encoinsTxTransferMode dWallet dCoins dSecretsWithNamesInTheWallet (pure Nothing) eSendToLedger dWalletSignature
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eWalletError, eStatusUpdate1, eStatusUpdate2]
    containerApp "" $ divClass "app-text-small" $ dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    sendButton dActive stl = divClass "menu-item-button-right" .
      btn (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive) stl . text

ledgerTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m ()
ledgerTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowLedgerStorageKey welcomeLedger
    (dBalance, dFees, dBulletproofParams, bRandomness) <- getEnvironment LedgerMode dWallet dAddr dToBurn dToMint

    containerApp "" $ transactionBalanceWidget dBalance dFees
    (dToBurn, dToMint, dAddr, eStatusUpdate) <- containerApp "" $
        divClassId "app-columns w-row" "welcome-ledger" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets $ zipDynWith (++) dOldSecrets dNewSecrets
            dSecretsWithNames <- coinCollectionWithNames dSecrets
            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 . toStrict . encode <$> updated dSecrets)

            (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-ledger-coins" $ do
                  mainWindowColumnHeader "Coins in the Ledger"
                  dSecretsWithNamesUniq <- holdUniqDyn dSecretsWithNamesInTheWallet
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesUniq
                  coinBurnCollectionWidget dSecretsWithNamesUniq
                eImp <- divClass "" $ do
                  (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
                  (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
                  exportWindow eExport dCTB
                  exportWindow eExportAll dSecrets
                  eIS    <- fmap pure . catMaybes <$> importWindow eImport
                  eISAll <- importFileWindow eImportAll
                  return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (dCoinsToMint, eSend, dChangeAddr) <- divClassId "app-column w-col w-col-6" "welcome-ledger-mint" $ mdo
                dCoinsToMint' <- divClass "" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost [AddCoin <$> eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate, AddCoin <$> eAddChange]
                    eNewSecret     <- coinNewWidget
                    return dCoinsToMint''
                eSend' <- sendRequestButton LedgerMode dStatus dWallet dCoinsToBurn dCoinsToMint (void $ updated dBalance)
                let dV = fmap calculateChange dBalance
                    dBalanceWithFees = zipDynWith (+) dBalance dFees
                    eSendZeroBalance = gate ((==0) <$> current dBalanceWithFees) eSend'
                    eSendNonZeroBalance = gate ((/=0) <$> current dBalanceWithFees) eSend'
                eAddChange <- coinNewButtonWidget dV never (addChangeButton dBalanceWithFees)
                (eAddrOk, dmAddr) <- inputAddressWindow eSendNonZeroBalance
                dAddr'          <- holdDyn Nothing (leftmost [updated dmAddr, Nothing <$ eSendZeroBalance])
                return (dCoinsToMint', leftmost [void eAddrOk, eSendZeroBalance], dAddr')
            (dAssetNamesInTheWallet, eStatusUpdate) <- encoinsTxLedgerMode dWallet dBulletproofParams bRandomness dChangeAddr dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, dChangeAddr, eStatusUpdate)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClass "app-text-small" $ dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    calculateChange bal = negate bal - protocolFees LedgerMode 0
    f v = if v < 0
      then "button-switching flex-center"
      else "button-not-selected button-disabled flex-center"
    addChangeButton dBal = btn (f <$> dBal) "margin-top: 10px;" $ text "ADD CHANGE"
    