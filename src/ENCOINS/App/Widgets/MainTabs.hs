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
import           Backend.Status                         (Status(..), walletError)
import           Backend.Types
import           Backend.Wallet                         (Wallet (..))
import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp)
import           ENCOINS.App.Widgets.Coin               (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                          coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget)
import           ENCOINS.App.Widgets.InputAddressWindow (inputAddressWindow)
import           ENCOINS.App.Widgets.ImportWindow       (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.SendRequestButton  (sendRequestButton)
import           ENCOINS.App.Widgets.SendToWalletWindow (sendToWalletWindow)
import           ENCOINS.App.Widgets.TransactionBalance (transactionBalanceWidget)
import           ENCOINS.App.Widgets.WelcomeWindow      (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey)
import           ENCOINS.Bulletproofs                   (Secrets, Secret (..))
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           ENCOINS.Crypto.Field                   (fromFieldElement)
import           JS.Website                             (logInfo, saveJSON)
import           Widgets.Utils                          (toText)

mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

walletTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m (Event t [(Secret, Text)])
walletTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
    dBalance <- holdUniqDyn $ zipDynWith (-) (getBalance dToBurn) (getBalance dToMint)
    containerApp "" $ transactionBalanceWidget WalletMode dBalance
    (dToBurn, dToMint, eStatusUpdate, _, ret) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
              updated dImportedSecrets
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets $ zipDynWith (++) dOldSecrets dNewSecrets
            dSecretsWithNames <- coinCollectionWithNames dSecrets
            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 .
              toStrict . encode <$> updated dSecrets)

            (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-wallet-coins" $ do
                  mainWindowColumnHeader "Coins in the Wallet"
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                  coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                eImp <- divClassId "" "welcome-import-export" $ do
                    (eImport, eImportAll) <- divClass "app-columns w-row" $
                      (,) <$> menuButton " Import" <*> menuButton " Import All"
                    (eExport, eExportAll) <- divClass "app-columns w-row" $
                      (,) <$> menuButton " Export" <*> menuButton " Export All"
                    exportWindow eExport dCTB
                    exportWindow eExportAll dSecrets
                    eIS <- fmap pure . catMaybes <$> importWindow eImport
                    eISAll <- importFileWindow eImportAll
                    return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                (dCoinsToMint', eNewSecret) <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret' <- coinNewWidget
                    return (dCoinsToMint'', eNewSecret')
                eSend' <- sendRequestButton WalletMode dBalance dStatus dWallet dCoinsToBurn dCoinsToMint
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate, dTxId) <- encoinsTxWalletMode dWallet dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate, dTxId, dSecretsWithNamesInTheWallet)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
    return (updated ret)
        -- let f txId s = bool blank (void $ lnk ("https://preprod.cexplorer.io/tx/" <> txId) "" $ divClass "text-footer" $ text txId) (s == Submitted)
        -- dyn_ $ f <$> dTxId <*> dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text

transferTab :: MonadWidget t m =>
    Maybe PasswordRaw -> Dynamic t Wallet -> Dynamic t [(Secret, Text)] ->
    Dynamic t Secrets -> m (Event t [(Secret, Text)])
transferTab mpass dWallet dSecretsWithNamesInTheWallet dOldSecrets = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        dBalance = zipDynWith (-) (getBalance $ pure []) (getBalance dCoins)
    containerApp "" $ transactionBalanceWidget TransferMode dBalance
    (dCoins, eSendToLedger, eAddr) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
          updated dImportedSecrets
        let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets dOldSecrets
        performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 .
          toStrict . encode <$> updated dSecrets)

        (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
            dCTB <- divClassId "" "welcome-coins-transfer" $ do
                mainWindowColumnHeader "Coins in the Wallet"
                dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                coinBurnCollectionWidget dSecretsWithNamesInTheWallet
            (eImport, eImportAll) <- divClass "app-columns w-row" $
              (,) <$> menuButton " Import" <*> menuButton " Import All"
            (eExport, eExportAll) <- divClass "app-columns w-row" $
              (,) <$> menuButton " Export" <*> menuButton " Export All"
            exportWindow eExport dCTB
            exportWindow eExportAll dSecrets
            eIS <- fmap pure . catMaybes <$> importWindow eImport
            eISAll <- importFileWindow eImportAll
            return (dCTB, leftmost [eIS, eISAll])
        divClassId "app-column w-col w-col-6" "welcome-transfer-btns" $ do
          eWallet <- sendButton (not . null <$> dCoinsToBurn) "" " Send to Wallet"
          eLedger <- sendButton (not . null <$> dCoinsToBurn) "margin-top: 20px" " Send to Ledger"
          eWalletOk <- sendToWalletWindow eWallet dCoinsToBurn
          eAddrOk <- inputAddressWindow eWalletOk
          return (dCoinsToBurn, eLedger, eAddrOk)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    (_, eStatusUpdate1, _) <- encoinsTxTransferMode "0" dWallet dCoins dSecretsWithNamesInTheWallet dAddr (void eAddr)
    (_, eStatusUpdate2, _) <- encoinsTxTransferMode "1" dWallet dCoins dSecretsWithNamesInTheWallet (pure Nothing) eSendToLedger
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eWalletError, eStatusUpdate1, eStatusUpdate2]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
    return never
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    sendButton dActive stl = divClass "menu-item-button-right" .
      btn (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive) stl . text

ledgerTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m (Event t [(Secret, Text)])
ledgerTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
    dBalance <- holdUniqDyn $ zipDynWith (-) (getBalance dToBurn) (getBalance dToMint)
    containerApp "" $ transactionBalanceWidget LedgerMode dBalance
    (dToBurn, dToMint, eStatusUpdate) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
              updated dImportedSecrets
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets $ zipDynWith (++) dOldSecrets dNewSecrets
            dSecretsWithNames <- coinCollectionWithNames dSecrets
            performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 .
              toStrict . encode <$> updated dSecrets)

            (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-wallet-coins" $ do
                  mainWindowColumnHeader "Coins in the Wallet"
                  dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                  coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                eImp <- divClassId "" "welcome-import-export" $ do
                  (eImport, eImportAll) <- divClass "app-columns w-row" $
                    (,) <$> menuButton " Import" <*> menuButton " Import All"
                  (eExport, eExportAll) <- divClass "app-columns w-row" $
                    (,) <$> menuButton " Export" <*> menuButton " Export All"
                  exportWindow eExport dCTB
                  exportWindow eExportAll dSecrets
                  eIS <- fmap pure . catMaybes <$> importWindow eImport
                  eISAll <- importFileWindow eImportAll
                  return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                (dCoinsToMint', eNewSecret) <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret' <- coinNewWidget
                    return (dCoinsToMint'', eNewSecret')
                eSend' <- sendRequestButton LedgerMode dBalance dStatus dWallet dCoinsToBurn dCoinsToMint
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate) <- encoinsTxLedgerMode dWallet dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
    return never
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
