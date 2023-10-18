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
import           Backend.Status                         (Status(..), isStatusBusyBackendNetwork)
import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp, walletError, elementResultJS, tellTxStatus)
import           ENCOINS.App.Widgets.Coin               (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,coinWithName,
                                                          filterKnownCoinNames, noCoinsFoundWidget, coinNewButtonWidget)
import           ENCOINS.App.Widgets.InputAddressWindow (inputAddressWindow)
import           ENCOINS.App.Widgets.ImportWindow       (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.SendRequestButton  (sendRequestButton)
import           ENCOINS.App.Widgets.SendToWalletWindow (sendToWalletWindow)
import           ENCOINS.App.Widgets.TransactionBalance (transactionBalanceWidget)
import           ENCOINS.App.Widgets.WelcomeWindow      (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey, welcomeLedger, welcomeWindowLedgerStorageKey)
import           ENCOINS.Bulletproofs                   (Secret)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           JS.Website                             (saveJSON)
import           Backend.Protocol.TxValidity (getDeposit)
import           ENCOINS.Common.Events


mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

walletTab :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t [(Secret, Text)]
  -> m ()
walletTab mpass dWallet dOldSecretsWithNames = sectionApp "" "" $ mdo
    (dBalance, dFees, dBulletproofParams, bRandomness) <-
        getEnvironment WalletMode dWallet (pure Nothing) dToBurn dToMint
    dTotalBalance <- holdUniqDyn $ zipDynWith (-) (negate <$> dBalance) dFees
    containerApp "" $ transactionBalanceWidget dTotalBalance dFees ""
    (dToBurn, dToMint, eStatusUpdate) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecretsWithNames = fmap nub
                  $ zipDynWith (++) dOldSecretsWithNames
                  $ map coinWithName
                  <$> zipDynWith (++) dImportedSecrets dNewSecrets

            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins-with-name" . decodeUtf8 . toStrict . encode <$> updated dSecretsWithNames)

            (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-wallet-coins" $ do
                  mainWindowColumnHeader "Coins in the Wallet"
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                  coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                eImp <- divClassId "" "welcome-import-export" $ do
                    (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
                    (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
                    exportWindow eExport dCTB
                    exportWindow eExportAll $ map fst <$> dSecretsWithNames
                    eIS    <- fmap pure . catMaybes <$> importWindow eImport
                    eISAll <- importFileWindow eImportAll
                    return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)

            (dCoinsToMint, eSend) <- divClass "app-CoinColumnRight w-col w-col-6" $ mdo
                dCoinsToMint' <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $
                      leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Constructing) eStatusUpdate]
                    eNewSecret <- coinNewWidget
                    return dCoinsToMint''
                eSend' <- sendRequestButton
                  WalletMode
                  dStatus
                  dWallet
                  dCoinsToBurn
                  dCoinsToMint
                  (void $ updated dBalance)
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate, _) <-
                encoinsTxWalletMode
                  dWallet
                  dBulletproofParams
                  bRandomness
                  dCoinsToBurn
                  dCoinsToMint
                  eSend
            let dSecretsWithNamesInTheWallet =
                  zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
    eWalletError <- walletError
    let eStatus = leftmost [eStatusUpdate, eWalletError]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Wallet status" Ready eStatus
  where
    menuButton = divClass "w-col w-col-6" .
      divClass "app-ImportExportButton" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text

transferTab :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t [(Secret, Text)]
  -> m ()
transferTab mpass dWallet dOldSecretsWithNames = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    dDepositBalance <- holdUniqDyn $ negate . getDeposit <$> dCoins
    containerApp "" $ transactionBalanceWidget (pure 0) (pure 0) " (to Wallet)"
    containerApp "" $ transactionBalanceWidget dDepositBalance (pure 0) " (to Ledger)"
    (dCoins, eSendToLedger, eAddr, dSecretsName) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        let dSecretsWithNames = nub <$> zipDynWith (++) dOldSecretsWithNames (map coinWithName <$> dImportedSecrets)
        performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins-with-name" . decodeUtf8 . toStrict . encode <$> updated dSecretsWithNames)

        (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
            dCTB <- divClassId "" "welcome-coins-transfer" $ do
                mainWindowColumnHeader "Coins in the Wallet"
                dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                coinBurnCollectionWidget dSecretsWithNamesInTheWallet
            (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
            (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
            exportWindow eExport dCTB
            exportWindow eExportAll $ map fst <$> dSecretsWithNames
            eIS    <- fmap pure . catMaybes <$> importWindow eImport
            eISAll <- importFileWindow eImportAll
            return (dCTB, leftmost [eIS, eISAll])
        divClassId "app-CoinColumnRight w-col w-col-6" "welcome-transfer-btns" $ do
          eWallet <- sendButton
            (zipDynWith
              (&&)
              (fmap (not . null) dCoinsToBurn)
              (fmap (not . isStatusBusyBackendNetwork) dStatus)
            ) "" " Send to Wallet"
          eLedger <- sendButton
            (zipDynWith
              (&&)
              (fmap (not . null) dCoinsToBurn)
              (fmap (not . isStatusBusyBackendNetwork) dStatus)
            ) "margin-top: 20px" " Send to Ledger"
          eWalletOk    <- sendToWalletWindow eWallet dCoinsToBurn
          (eAddrOk, _) <- inputAddressWindow eWalletOk
          return (dCoinsToBurn, eLedger, eAddrOk, dSecretsWithNames)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    (dAssetNamesInTheWallet, eStatusUpdate1, _) <-
      encoinsTxTransferMode
        dWallet
        dCoins
        dSecretsWithNamesInTheWallet
        dAddr
        (void eAddr)
        dWalletSignature

    let dSecretsWithNamesInTheWallet =
          zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsName
    (_, eStatusUpdate2, _) <-
      encoinsTxTransferMode
        dWallet
        dCoins
        dSecretsWithNamesInTheWallet
        (pure Nothing)
        eSendToLedger
        dWalletSignature

    eWalletError <- walletError
    let eStatus = leftmost [eWalletError, eStatusUpdate1, eStatusUpdate2]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Transfer status" Ready eStatus
  where
    menuButton = divClass "w-col w-col-6" .
      divClass "app-ImportExportButton" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    sendButton dActive stl = divClass "app-SendTransferButton" .
      btn (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive) stl . text

ledgerTab :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t Wallet
  -> Dynamic t [(Secret, Text)]
  -> m ()
ledgerTab mpass dWallet dOldSecretsWithNames = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowLedgerStorageKey welcomeLedger
    (dBalance, dFees, dBulletproofParams, bRandomness) <-
        getEnvironment LedgerMode dWallet dAddr dToBurn dToMint

    dDepositBalance <- holdUniqDyn $
      zipDynWith (-) (getDeposit <$> dToBurn) (getDeposit <$> dToMint)
    dEncoinsDepositBalance <- holdUniqDyn $ zipDynWith (+) (negate <$> dBalance) dDepositBalance
    dTotalBalance <- holdUniqDyn $ zipDynWith (-) dEncoinsDepositBalance dFees
    logDyn "dDepositBalance" dDepositBalance
    logDyn "dEncoinsDepositBalance" dEncoinsDepositBalance
    logDyn "dTotalBalance" dTotalBalance
    containerApp "" $ transactionBalanceWidget dTotalBalance dFees ""

    (dToBurn, dToMint, dAddr, eStatusUpdate) <- containerApp "" $
        divClassId "app-columns w-row" "welcome-ledger" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecretsWithNames = fmap nub $ zipDynWith (++) dOldSecretsWithNames $ map coinWithName <$> zipDynWith (++) dImportedSecrets dNewSecrets
            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins-with-name" . decodeUtf8 . toStrict . encode <$> updated dSecretsWithNames)

            (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-ledger-coins" $ do
                  mainWindowColumnHeader "Coins in the Ledger"
                  dSecretsWithNamesUniq <- holdUniqDyn dSecretsWithNamesInTheWallet
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesUniq
                  coinBurnCollectionWidget dSecretsWithNamesUniq
                eImp <- divClass "" $ do
                  (eImport, eImportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Import" <*> menuButton " Import All"
                  (eExport, eExportAll) <- divClass "app-columns w-row" $ (,) <$> menuButton " Export" <*> menuButton " Export All"
                  exportWindow eExport dCTB
                  exportWindow eExportAll $ map fst <$> dSecretsWithNames
                  eIS    <- fmap pure . catMaybes <$> importWindow eImport
                  eISAll <- importFileWindow eImportAll
                  return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (dCoinsToMint, eSend, dChangeAddr) <- divClassId "app-CoinColumnRight w-col w-col-6" "welcome-ledger-mint" $ mdo
                dCoinsToMint' <- divClass "" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost [AddCoin <$> eNewSecret, ClearCoins <$ ffilter (== Constructing) eStatusUpdate, AddCoin <$> eAddChange]
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
    let eStatus = leftmost [eStatusUpdate, eWalletError]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Ledger status" Ready eStatus
  where
    menuButton = divClass "w-col w-col-6" .
      divClass "app-ImportExportButton" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    calculateChange bal = negate bal - protocolFees LedgerMode 0
    f v = if v < 0
      then "button-switching flex-center"
      else "button-not-selected button-disabled flex-center"
    addChangeButton dBal = btn (f <$> dBal) "margin-top: 10px;" $ text "ADD CHANGE"
