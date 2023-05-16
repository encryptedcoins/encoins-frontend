{-# LANGUAGE NumericUnderscores #-}

module ENCOINS.App.Widgets.MainWindow where

import           Control.Monad                    ((<=<), void)
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

import           Backend.EncoinsTx                (encoinsTxWalletMode, encoinsTxTransferMode, encoinsTxLedgerMode, secretToHex)
import           Backend.Servant.Client           (pabIP)
import           Backend.Servant.Requests         (statusRequestWrapper)
import           Backend.Status                   (Status(..), walletError)
import           Backend.Types
import           Backend.Wallet                   (Wallet (..), WalletName (..))
import           CSL                              (TransactionUnspentOutput(..), amount, coin)
import           ENCOINS.App.Widgets.Basic        (containerApp, sectionApp, elementResultJS)
import           ENCOINS.App.Widgets.Coin         (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                    coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget)
import           ENCOINS.App.Widgets.ImportWindow (importWindow, importFileWindow, exportWindow)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw(..))
import           ENCOINS.App.Widgets.WelcomeWindow (welcomeWindow, welcomeTransfer, welcomeWindowTransferStorageKey)
import           ENCOINS.Bulletproofs             (Secrets, Secret (..))
import           ENCOINS.Common.Widgets.Basic     (btn, br, divClassId, errDiv)
import           ENCOINS.Common.Widgets.Advanced  (dialogWindow)
import           ENCOINS.Crypto.Field             (fromFieldElement)
import           JS.App                           (addrLoad)
import           JS.Website                       (logInfo, saveJSON)
import           Widgets.Utils                    (toText)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Integer -> m ()
transactionBalanceWidget balance =
    let balanceSign n = bool "-" "+" (n >= 0)
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

sendRequestButton :: MonadWidget t m => Dynamic t Status -> Dynamic t Wallet ->
  Dynamic t Secrets -> Dynamic t Secrets -> Dynamic t (Maybe Integer) ->
  m (Event t ())
sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint dBalOk = do
    let dTxValidity = txValidity <$> dStatus <*> dWallet <*> dCoinsToBurn <*> dCoinsToMint
        f v b = case (v,b) of
            (TxValid, Nothing) -> "button-switching flex-center"
            _ -> "button-not-selected button-disabled flex-center"
        g v b = case (v,b) of
            (TxValid, Nothing) -> blank
            (TxInvalid e, _) -> elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
                "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $
                divClass "app-text-normal" $ text e
            (_, Just n) -> elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
                "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $
                divClass "app-text-normal" $ text $ "Cannot withdraw more than "
                  <> toText n <> " ADA in one transaction."
        h v b = case (v,b) of
            (TxValid, Nothing) -> ""
            _ -> "border-bottom-left-radius: 0px; border-bottom-right-radius: 0px"
    e <- divClassId "" "welcome-send-req" $ btn (zipDynWith f dTxValidity dBalOk)
        (zipDynWith h dTxValidity dBalOk) $ dynText "SEND REQUEST"
    dyn_ $ zipDynWith g dTxValidity dBalOk
    return $ () <$ ffilter (== (TxValid, Nothing))
      (current (zipDyn dTxValidity dBalOk) `tag` e)

data AppTab = WalletTab | TransferTab | LedgerTab deriving (Eq, Show)

tabsSection :: MonadWidget t m => Dynamic t AppTab -> m (Event t AppTab)
tabsSection dTab = sectionApp "" "" $ containerApp "" $
    divClassId "app-top-menu-div" "welcome-tabs"$ do
        eWallet <- divClass "menu-item-button-right" $
            btn (mkBtnCls WalletTab <$> dTab) "" $ text "Wallet"
        eTransfer <- divClass "menu-item-button-right" $
            btn (mkBtnCls TransferTab <$> dTab) "" $ text "Transfer"
        eLedger <- divClass "menu-item-button-right" $
            btn (mkBtnCls LedgerTab <$> dTab) "" $ text "Ledger"
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

mainWindow :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m ()
mainWindow mpass dWallet dOldSecrets = mdo
    eTab <- tabsSection dTab
    dTab <- holdDyn WalletTab eTab

    eSecretsWithNamesInTheWallet <- switchHold never <=< dyn $ dTab <&> \case
      WalletTab -> walletTab mpass dWallet dOldSecrets
      TransferTab -> transferTab mpass dWallet dSecretsWithNamesInTheWallet
        dOldSecrets
      LedgerTab -> ledgerTab mpass dWallet
    dSecretsWithNamesInTheWallet <- holdDyn [] eSecretsWithNamesInTheWallet

    blank

walletTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  Dynamic t Secrets -> m (Event t [(Secret, Text)])
walletTab mpass dWallet dOldSecrets = sectionApp "" "" $ mdo
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        getMaxAda (MaxAdaWithdrawResult n) = Just n
        getMaxAda _ = Nothing
    dBalance <- holdUniqDyn $ zipDynWith (-) (getBalance dToBurn) (getBalance dToMint)
    baseUrl <- pabIP
    (eMaxAda, _) <- statusRequestWrapper baseUrl (pure MaxAdaWithdraw)
      (void $ updated dBalance)
    dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)
    let balanceOk bal maxAda = if bal + maxAda >= 0 then Nothing else Just maxAda
        dBalanceOk = zipDynWith balanceOk dBalance dMaxAda
    containerApp "" $ transactionBalanceWidget dBalance
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
                eSend' <- sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint dBalanceOk
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
    menuButton = divClass "menu-item-button-right" .
      btn "button-switching flex-center" "margin-top: 20px" . text

transferTab :: MonadWidget t m =>
    Maybe PasswordRaw -> Dynamic t Wallet -> Dynamic t [(Secret, Text)] ->
    Dynamic t Secrets -> m (Event t [(Secret, Text)])
transferTab mpass dWallet dSecretsWithNamesInTheWallet dOldSecrets = sectionApp "" "" $ mdo
    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        dBalance = zipDynWith (-) (getBalance $ pure []) (getBalance dCoins)
    containerApp "" $ transactionBalanceWidget dBalance
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
          eAddrOk <- inputAddrDialog eWalletOk
          return (dCoinsToBurn, eLedger, eAddrOk)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    (_, eStatusUpdate1, _) <- encoinsTxTransferMode dWallet dCoins dAddr (void eAddr)
    (_, eStatusUpdate2, _) <- encoinsTxTransferMode dWallet dCoins (pure Nothing) eSendToLedger
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

inputAddrDialog :: MonadWidget t m => Event t () -> m (Event t Address)
inputAddrDialog eOpen = mdo
  eOk <- dialogWindow True eOpen (void eOk) "width: 950px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ do
      divClass "connect-title-div" $ divClass "app-text-semibold" $
          text "Enter wallet address in bech32:"
      dAddrInp <- divClass "app-columns w-row" $ do
        inp <- inputElement $ def
          & initialAttributes .~ ("class" =: "w-input" <> "style" =: "display: inline-block;")
          & inputElementConfig_initialValue .~ ""
          & inputElementConfig_setValue .~ ("" <$ eOpen)
        return (value inp)
      btnOk <- btn "button-switching inverted flex-center" "width:30%;display:inline-block;margin-right:5px;" $ text "Ok"
      performEvent_ (addrLoad <$> tagPromptlyDyn dAddrInp btnOk)
      dPubKeyHash <- elementResultJS "addrPubKeyHashElement" id
      dStakeKeyHash <- elementResultJS "addrStakeKeyHashElement" id
      let
        dmAddr = zipDynWith mkAddr (checkEmptyText <$> dPubKeyHash) (checkEmptyText <$> dStakeKeyHash)
        emRes = tagPromptlyDyn dmAddr btnOk
      widgetHold_ blank $ leftmost [maybe err (const blank) <$> emRes, blank <$ eOpen]
      return (catMaybes emRes)
  return eOk
  where
    mkAddr Nothing _ = Nothing
    mkAddr (Just pkh) mskh = Just $ mkAddressFromPubKeys pkh mskh
    err = elAttr "div" ("class" =: "app-columns w-row" <>
      "style" =: "display:flex;justify-content:center;") $
        errDiv "Incorrect address"

ledgerTab :: MonadWidget t m => Maybe PasswordRaw -> Dynamic t Wallet ->
  m (Event t [(Secret, Text)])
ledgerTab mpass dWallet = sectionApp "" "" $ mdo
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        getMaxAda (MaxAdaWithdrawResult n) = Just n
        getMaxAda _ = Nothing
    dBalance <- holdUniqDyn $ zipDynWith (-) (getBalance dToBurn) (getBalance dToMint)
    baseUrl <- pabIP
    (eMaxAda, _) <- statusRequestWrapper baseUrl (pure MaxAdaWithdraw)
      (void $ updated dBalance)
    dMaxAda <- holdDyn 0 (mapMaybe getMaxAda eMaxAda)
    let balanceOk bal maxAda = if bal + maxAda >= 0 then Nothing else Just maxAda
        dBalanceOk = zipDynWith balanceOk dBalance dMaxAda
    containerApp "" $ transactionBalanceWidget dBalance
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
                eSend' <- sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint dBalanceOk
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
    menuButton = divClass "menu-item-button-right" .
      btn "button-switching flex-center" "margin-top: 20px" . text
    dOldSecrets = pure []
