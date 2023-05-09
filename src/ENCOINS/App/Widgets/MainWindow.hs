{-# LANGUAGE NumericUnderscores #-}

module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                    ((<=<))
import           Data.Aeson                       (encode)
import           Data.Bool                        (bool)
import           Data.ByteString.Lazy             (toStrict)
import           Data.Functor                     ((<&>))
import           Data.List                        (nub)
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Data.Text.Encoding               (decodeUtf8)
import           PlutusTx.Prelude                 (divide)
import           Reflex.Dom
import           Witherable                       (catMaybes)

import           Backend.EncoinsTx                (encoinsTx, encoinsTxWallet, encoinsTxLedger)
import           Backend.Status                   (Status(..), walletError)
import           Backend.Wallet                   (Wallet (..), WalletName (..))
import           CSL                              (TransactionUnspentOutput(..), amount, coin)
import           ENCOINS.App.Widgets.Basic        (containerApp, sectionApp, loadAppData)
import           ENCOINS.App.Widgets.Coin         (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                    coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget, secretToHex)
import           ENCOINS.App.Widgets.ImportWindow (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw(..))
import           ENCOINS.App.Widgets.WelcomeWindow (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey)
import           ENCOINS.Bulletproofs             (Secrets, Secret (..))
import           ENCOINS.Common.Widgets.Basic     (btn, br, divClassId)
import           ENCOINS.Common.Widgets.Advanced  (dialogWindow)
import           ENCOINS.Crypto.Field             (fromFieldElement)
import           JS.Website                       (logInfo, saveJSON)
import           Widgets.Utils                    (toText)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Secrets -> Dynamic t Secrets -> m ()
transactionBalanceWidget dCoinsToBurn dCoinsToMint =
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        balance = zipDynWith (-) (getBalance dCoinsToBurn) (getBalance dCoinsToMint)
        balanceSign n = bool "-" "+" (n >= 0)
        balanceADA n = "Transaction balance: " <> balanceSign n <> toText (abs n) <> " ADA"
        feeADA n = bool blank (divClass "app-text-semibold" $ text $ "Fee: " <> toText (max 3 $ n `divide` 100) <> " ADA") (n > 0)
    in divClassId "transaction-balance-div" "welcome-tx-balance" $ do
        divClass "app-text-semibold" $ dynText $ fmap balanceADA balance
        dyn_ $ fmap feeADA balance

mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

data TxValidity = TxValid | TxInvalid Text
    deriving (Show, Eq)

instance Semigroup TxValidity where
    TxValid <> TxValid = TxValid
    TxValid <> TxInvalid e = TxInvalid e
    TxInvalid e <> TxValid = TxInvalid e
    TxInvalid e1 <> TxInvalid _ = TxInvalid e1

instance Monoid TxValidity where
    mempty = TxValid

txValidity :: Status -> Wallet -> Secrets -> Secrets -> TxValidity
txValidity s Wallet{..} toBurn toMint = mconcat $ zipWith f
        [e7, e8, e6, e0, e1, e2, e3, e4, e5]
        [cond7, cond8, cond6, cond0, cond1, cond2, cond3, cond4, cond5]
    where
        getBalance = sum . map (fromFieldElement . secretV)
        f e = bool (TxInvalid e) TxValid
        coins = toBurn ++ toMint
        cond0 = s `notElem` [Balancing, Signing, Submitting, Submitted]
        cond1 = walletName /= None
        cond2 = not $ null toMint
        cond3 = length coins >= 2
        cond4 = length coins <= 5
        cond5 = length coins == length (nub coins)
        cond6 = (getBalance toMint - getBalance toBurn + 5) * 1_000_000 < sum (map (fromJust . decodeText . coin . amount . output) walletUTXOs)
        cond7 = walletName /= None
        cond8 = walletNetworkId == "0"
        e0    = "The transaction is being processed."
        e1    = "Connect ENCOINS DApp to a wallet first."
        e2    = "Minting at least one coin is required to preserve privacy."
        e3    = "At least two coins must be included in a transaction to preserve privacy."
        e4    = "At most five coins can be included in a transaction."
        e5    = "A transaction cannot include coin duplicates."
        e6    = "Not enough ADA."
        e7    = "Connect ENCOINS to a wallet first."
        e8    = "Switch to the Testnet Preprod network in your wallet."

sendRequestButton :: MonadWidget t m => Dynamic t Status -> Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> m (Event t ())
sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint = do
    let dTxValidity = txValidity <$> dStatus <*> dWallet <*> dCoinsToBurn <*> dCoinsToMint
        f v = case v of
            TxValid     -> "button-switching flex-center"
            TxInvalid _ -> "button-not-selected button-disabled flex-center"
        g v = case v of
            TxValid     -> blank
            TxInvalid e -> elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
                "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $
                divClass "app-text-normal" $ text e
        h v = case v of
            TxValid     -> ""
            TxInvalid _ -> "border-bottom-left-radius: 0px; border-bottom-right-radius: 0px"
    e <- divClassId "" "welcome-send-req" $ btn (f <$> dTxValidity)
        (h <$> dTxValidity) $ dynText "SEND REQUEST"
    dyn_ $ fmap g dTxValidity
    return $ () <$ ffilter (== TxValid) (current dTxValidity `tag` e)

data AppTab = WalletTab | TransferTab | LedgerTab deriving (Eq, Show)

tabsSection :: MonadWidget t m => Dynamic t AppTab -> m (Event t AppTab)
tabsSection dTab = sectionApp "" "" $ containerApp "" $
    divClassId "app-top-menu-div" "welcome-tabs"$ do
        eWallet <- divClass "menu-item-button-right" $
            btn (mkBtnCls WalletTab <$> dTab) "" $ text "Wallet"
        eTransfer <- divClass "menu-item-button-right" $
            btn (mkBtnCls TransferTab <$> dTab) "" $ text "Transfer"
        eLedger <- divClass "menu-item-button-right" $
            btn "button-not-selected button-disabled" "" $ text "Ledger"
        return $ leftmost
            [ WalletTab <$ eWallet
            , TransferTab <$ eTransfer
            , LedgerTab <$ eLedger ]
        -- e <- divClass "menu-item-button-right" $
        --     btn "button-switching flex-center" "" $ do
        --         -- void $ image "import.svg" "image-button inverted" "30px"
        --         dynText " Import"
        -- importWindow e
    where
        mkBtnCls val cur = bool "button-not-selected" "" (val == cur)

mainWindow :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet -> m ()
mainWindow mpass dWallet = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab

    eSecretsWithNamesInTheWallet <- switchHold never <=< dyn $ dTab <&> \case
      WalletTab -> walletTab mpass dWallet
      TransferTab -> transferTab mpass dWallet dSecretsWithNamesInTheWallet
      LedgerTab -> pure never
    dSecretsWithNamesInTheWallet <- holdDyn [] eSecretsWithNamesInTheWallet

    blank

walletTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet -> m (Event t [(Secret, Text)])
walletTab mpass dWallet = sectionApp "" "" $ mdo
    containerApp "" $ transactionBalanceWidget dToBurn dToMint
    (dToBurn, dToMint, eStatusUpdate, _, ret) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
              updated dImportedSecrets
            dOldSecrets <- loadAppData (getPassRaw <$> mpass) "encoins" id []
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
                    eImport <- menuButton " Import"
                    eImportAll <- menuButton " Import All"
                    eExport <- menuButton " Export"
                    exportWindow eExport dCTB
                    eExportAll <- menuButton " Export All"
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
                eSend' <- sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint
                return (dCoinsToMint', eSend')
            (dAssetNamesInTheWallet, eStatusUpdate, dTxId) <- encoinsTx dWallet dCoinsToBurn dCoinsToMint eSend
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
    menuButton = divClass "menu-item-button-right" .
      btn "button-switching flex-center" "margin-top: 20px" . text

transferTab :: MonadWidget t m =>
    Maybe PasswordRaw -> Dynamic t Wallet -> Dynamic t [(Secret, Text)] -> m (Event t [(Secret, Text)])
transferTab mpass dWallet dSecretsWithNamesInTheWallet = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    containerApp "" $ transactionBalanceWidget (pure []) dCoins
    (dCoins, eSendToWallet, eSendToLedger) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        performEvent_ $ logInfo . ("dImportedSecrets: "<>) . toText <$>
          updated dImportedSecrets
        dOldSecrets <- loadAppData (getPassRaw <$> mpass) "encoins" id []
        let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets dOldSecrets
        performEvent_ (saveJSON (getPassRaw <$> mpass) "encoins" . decodeUtf8 .
          toStrict . encode <$> updated dSecrets)

        (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
            dCTB <- divClassId "" "welcome-coins-transfer" $ do
                mainWindowColumnHeader "Coins in the Wallet"
                dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                coinBurnCollectionWidget dSecretsWithNamesInTheWallet
            eImport <- menuButton " Import"
            eImportAll <- menuButton " Import All"
            eExport <- menuButton " Export"
            exportWindow eExport dCTB
            eExportAll <- menuButton " Export All"
            exportWindow eExportAll dSecrets
            eIS <- fmap pure . catMaybes <$> importWindow eImport
            eISAll <- importFileWindow eImportAll
            return (dCTB, leftmost [eIS, eISAll])
        divClassId "app-column w-col w-col-6" "welcome-transfer-btns" $ do
          eWallet <- sendButton (not . null <$> dCoinsToBurn) "" " Send to Wallet"
          eLedger <- sendButton (not . null <$> dCoinsToBurn) "margin-top: 20px" " Send to Ledger"
          eWalletOk <- sendToWalletDialog eWallet dCoinsToBurn
          return (dCoinsToBurn, eWalletOk, eLedger)
    (_, eStatusUpdate1, _) <- encoinsTxWallet dWallet dCoins eSendToWallet
    (_, eStatusUpdate2, _) <- encoinsTxLedger dWallet dCoins eSendToLedger
    eWalletError <- walletError
    dStatus <- holdDyn Ready $ leftmost [eWalletError, eStatusUpdate1, eStatusUpdate2]
    containerApp "" $ divClass "app-text-small" $ do
        dynText $ fmap toText dStatus
    return never
  where
    menuButton = divClass "menu-item-button-right" .
      btn "button-switching flex-center" "margin-top: 20px" . text
    sendButton dActive stl = divClass "menu-item-button-right" .
      btn (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive) stl . text

sendToWalletDialog :: MonadWidget t m => Event t () -> Dynamic t Secrets -> m (Event t ())
sendToWalletDialog eOpen dSecrets = mdo
  (eOk, eCancel) <- dialogWindow True eOpen (leftmost [eOk,eCancel]) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ do
      divClass "connect-title-div" $ divClass "app-text-semibold" $
          text "Copy and send these keys to your recepient off-chain:"
      elAttr "div" ("class" =: "app-text-normal" <> "style" =: "justify-content: space-between;text-align:left;") $
          dyn_ $ mapM ((>> br) . text . secretToHex) <$> dSecrets
      br
      btnOk <- btn "button-switching inverted flex-center" "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      btnCancel <- btn "button-switching flex-center" "width:30%;display:inline-block;margin-left:5px;" $ text "Cancel"
      return (btnOk, btnCancel)
  return eOk
