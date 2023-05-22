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
import           ENCOINS.App.Widgets.Basic              (containerApp, sectionApp, elementResultJS)
import           ENCOINS.App.Widgets.Coin               (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                          coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget, coinNewButtonWidget)
import           ENCOINS.App.Widgets.InputAddressWindow (inputAddressWindow)
import           ENCOINS.App.Widgets.ImportWindow       (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow     (PasswordRaw(..))
import           ENCOINS.App.Widgets.SendRequestButton  (sendRequestButton)
import           ENCOINS.App.Widgets.SendToWalletWindow (sendToWalletWindow)
import           ENCOINS.App.Widgets.TransactionBalance (transactionBalanceWidget)
import           ENCOINS.App.Widgets.WelcomeWindow      (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey)
import           ENCOINS.Bulletproofs                   (Secrets, Secret (..), Randomness)
import           ENCOINS.Common.Widgets.Basic           (btn, divClassId)
import           ENCOINS.Crypto.Field                   (fromFieldElement)
import           JS.Website                             (logInfo, saveJSON)
import           Widgets.Utils                          (toText)

mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

walletTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> Behavior t Randomness -> m ()
walletTab mpass dWallet dOldSecrets bRandomness = sectionApp "" "" $ mdo
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
    dBalance <- holdUniqDyn $ zipDynWith (-) (getBalance dToBurn) (getBalance dToMint)
    containerApp "" $ transactionBalanceWidget WalletMode dBalance
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
                dCoinsToMint' <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost
                      [ fmap AddCoin eNewSecret
                      , ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret <- coinNewWidget
                    return dCoinsToMint''
                eSend' <- sendRequestButton WalletMode dBalance dStatus dWallet dCoinsToBurn dCoinsToMint
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate, _) <- encoinsTxWalletMode dWallet bRandomness dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text

transferTab :: MonadWidget t m =>
    Maybe PasswordRaw -> Dynamic t Wallet ->
    Dynamic t Secrets -> m ()
transferTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        dBalance = zipDynWith (-) (getBalance $ pure []) (getBalance dCoins)
    containerApp "" $ transactionBalanceWidget TransferMode dBalance
    (dCoins, eSendToLedger, eAddr, dSecretsWithNames) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
          updated dImportedSecrets
        let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets dOldSecrets
        dSecretsWithNames <- coinCollectionWithNames dSecrets
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
          return (dCoinsToBurn, eLedger, eAddrOk, dSecretsWithNames)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    (dAssetNamesInTheWallet, eStatusUpdate1, _) <- encoinsTxTransferMode dWallet dCoins dSecretsWithNamesInTheWallet dAddr (void eAddr) dWalletSignature
    let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
    (_, eStatusUpdate2, _) <- encoinsTxTransferMode dWallet dCoins dSecretsWithNamesInTheWallet (pure Nothing) eSendToLedger dWalletSignature
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eWalletError, eStatusUpdate1, eStatusUpdate2]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    sendButton dActive stl = divClass "menu-item-button-right" .
      btn (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive) stl . text

ledgerTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> Behavior t Randomness -> m ()
ledgerTab mpass dWallet dOldSecrets bRandomness = sectionApp "" "" $ mdo
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
            (dCoinsToMint, eSend, dChangeAddr) <- divClass "app-column w-col w-col-6" $ mdo
                dCoinsToMint' <- divClassId "" "welcome-coins-mint" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint'' <- coinMintCollectionWidget $ leftmost
                      [ AddCoin <$> eNewSecret
                      , ClearCoins <$ ffilter (== Balancing) eStatusUpdate
                      , AddCoin <$> eAddChange ]
                    eNewSecret <- coinNewWidget
                    return dCoinsToMint''
                eSend' <- sendRequestButton LedgerMode dBalance dStatus dWallet dCoinsToBurn dCoinsToMint
                eAddChange <- coinNewButtonWidget dBalance never
                  (addChangeButton dBalance)
                let
                  eSendZeroBalance = gate ((==0) <$> current dBalance) eSend'
                  eSendNonZeroBalance = gate ((/=0) <$> current dBalance) eSend'
                eAddr <- inputAddressWindow eSendNonZeroBalance
                dAddr' <- holdDyn Nothing (Just <$> eAddr)
                return (dCoinsToMint', leftmost [eSendZeroBalance, void eAddr], dAddr')
            (dAssetNamesInTheWallet, eStatusUpdate) <- encoinsTxLedgerMode dWallet bRandomness dChangeAddr dCoinsToBurn dCoinsToMint eSend
            let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
            return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
  where
    menuButton = divClass "app-column w-col w-col-6" .
      divClass "menu-item-button-right" . btn "button-switching flex-center"
        "margin-top:20px;min-width:unset" . text
    addChangeButton dBal = btn (f <$> dBal) "margin-top: 10px;"
      $ text "ADD CHANGE"
    f v = if (v > 0)
      then "button-switching flex-center"
      else "button-not-selected button-disabled flex-center"
