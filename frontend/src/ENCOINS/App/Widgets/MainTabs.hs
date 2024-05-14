{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MainTabs where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Text (Text)
import Reflex.Dom

import Backend.EncoinsTx
    ( encoinsTxLedgerMode
    , encoinsTxTransferMode
    , encoinsTxWalletMode
    )
import Backend.Utility (nubWith)
import Backend.Environment (getEnvironment)
import Backend.Protocol.Setup (emergentChangeAddress, ledgerAddress)
import Backend.Protocol.TxValidity (getAda, getCoinNumber, getDeposit)
import Backend.Protocol.Types
import Backend.Servant.Requests (currentRequestWrapper)
import Backend.Status (AppStatus, Status (..), isStatusWantBlockButtons)
import Backend.Wallet (Wallet (..))
import Config.Config (delegateServerUrl)
import ENCOINS.App.Widgets.Basic
    ( containerApp
    , elementResultJS
    , saveAppData
    , sectionApp
    , tellTxStatus
    , walletError
    )
import ENCOINS.App.Widgets.Cloud
import ENCOINS.App.Widgets.Coin
    ( CoinUpdate (..)
    , coinBurnCollectionWidget
    , coinMintCollectionV3Widget
    , coinNewButtonWidget
    , coinNewWidget
    , coinV3
    , filterByKnownCoinNames
    , noCoinsFoundWidget
    )
import ENCOINS.App.Widgets.ImportWindow (exportWindow, importWindow)
import ENCOINS.App.Widgets.InputAddressWindow (inputAddressWindow)
import ENCOINS.App.Widgets.SendRequestButton
    ( sendRequestButtonLedger
    , sendRequestButtonWallet
    )
import ENCOINS.App.Widgets.SendToWalletWindow (sendToWalletWindow)
import ENCOINS.App.Widgets.TransactionBalance
    ( Formula (..)
    , transactionBalanceWidget
    )
import ENCOINS.App.Widgets.WelcomeWindow
    ( welcomeLedger
    , welcomeTransfer
    , welcomeWindow
    , welcomeWindowLedgerStorageKey
    , welcomeWindowTransferStorageKey
    )
import ENCOINS.Common.Cache (encoinsV3)
import ENCOINS.Common.Events
import ENCOINS.Common.Widgets.Basic (btn, divClassId)

mainWindowColumnHeader :: (MonadWidget t m) => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $
            text title

walletTab ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Dynamic t Wallet
    -> Dynamic t [TokenCacheV3] -- consider use Map or Set
    -> Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
walletTab mpass dWallet dTokenCacheOld dCloudOn dmKey eWasMigration = sectionApp "" "" $ mdo
    eFetchUrls <- newEvent
    eeUrls <- currentRequestWrapper delegateServerUrl eFetchUrls
    let (eUrlError, eUrls) = (filterLeft eeUrls, filterRight eeUrls)
    dUrls <- holdDyn [] eUrls

    (dBalance, dFees, dBulletproofParams, bRandomness) <-
        getEnvironment
            WalletMode
            (fmap walletChangeAddress dWallet)
            dToBurn
            dToMint
    dTotalBalance <- holdUniqDyn $ zipDynWith (-) (negate <$> dBalance) dFees
    let formula =
            Formula
                dTotalBalance
                dFees
                (getAda <$> dToBurn)
                (getAda <$> dToMint)
                0
                0
    containerApp "" $ transactionBalanceWidget formula (Just WalletMode) ""
    (dToBurn, dToMint, eStatusUpdate, dNewTokensV3) <- containerApp "" $
        divClass "app-columns w-row" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dTokenCache =
                    fmap (nubWith tcAssetName) $
                        zipDynWith (++) dTokenCacheOld $
                            map coinV3
                                <$> zipDynWith (++) dImportedSecrets dNewSecrets
            -- logDyn "walletTab: dTokenCache" $ showTokens <$> dTokenCache

            -- Save begin
            dTokensUpdated <- handleUnsavedTokens dCloudOn dmKey dTokenCache eWasMigration
            -- Save end

            -- save unique changes only
            dTokensUpdatedUniq <- holdUniqDyn dTokensUpdated
            saveCacheLocally mpass "walletTab" dTokensUpdatedUniq

            (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-wallet-coins" $ do
                    mainWindowColumnHeader "Coins in the Wallet"
                    dSecretsUniq <- holdUniqDyn dSecretsInTheWallet
                    dyn_ $ fmap noCoinsFoundWidget dSecretsUniq
                    coinBurnCollectionWidget dSecretsUniq
                eImp <- divClassId "" "welcome-import-export" $ do
                    (eImport, eExport) <-
                        divClass "app-columns w-row" $
                            (,) <$> menuButton "Import" <*> menuButton "Export"
                    exportWindow eExport dCTB (map tcSecret <$> dTokenCache)
                    (eIS, eISAll) <- importWindow eImport
                    pure $ leftmost [eIS, eISAll]
                pure (dCTB, eImp)

            (dCoinsToMint, eSend, eSendStatus) <-
                divClass "app-CoinColumnRight w-col w-col-6" $ mdo
                    dCoinsToMint' <- divClassId "" "welcome-coins-mint" $ mdo
                        mainWindowColumnHeader "Coins to Mint"
                        dCoinsToMint'' <-
                            coinMintCollectionV3Widget $
                                leftmost
                                    [ fmap AddCoin eNewSecret
                                    , ClearCoins <$ ffilter (== Ready) eStatusUpdate
                                    ]
                        eNewSecret <- coinNewWidget
                        return dCoinsToMint''
                    (eSendStatus, eValidTx) <-
                        sendRequestButtonWallet
                            WalletMode
                            dStatus
                            dWallet
                            dCoinsToBurn
                            dCoinsToMint
                            (void $ updated dBalance)
                            dUrls
                    return (dCoinsToMint', eValidTx, eSendStatus)
            (dAssetNamesInTheWallet, eStatusUpdate, _) <-
                encoinsTxWalletMode
                    dWallet
                    dBulletproofParams
                    bRandomness
                    dCoinsToBurn
                    dCoinsToMint
                    eSend
                    dUrls
            let dSecretsInTheWallet =
                    zipDynWith
                        filterByKnownCoinNames
                        dAssetNamesInTheWallet
                        dTokensUpdated

            pure
                ( dCoinsToBurn
                , dCoinsToMint
                , leftmost [eStatusUpdate, eSendStatus]
                , dTokensUpdated
                )
    eWalletError <- walletError
    let eStatus = leftmost [eStatusUpdate, eWalletError, NoRelay <$ eUrlError]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Wallet mode" eStatus
    pure dNewTokensV3
    where
        menuButton =
            divClass "w-col w-col-6"
                . divClass "app-ImportExportButton"
                . btn
                    "button-switching flex-center"
                    "margin-top:20px;min-width:unset"
                . text

transferTab ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Dynamic t Wallet
    -> Dynamic t [TokenCacheV3]
    -> Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
transferTab mpass dWallet dTokenCacheOld dCloudOn dmKey eWasMigration = sectionApp "" "" $ mdo
    eFetchUrls <- newEvent
    eeUrls <- currentRequestWrapper delegateServerUrl eFetchUrls
    let (eUrlError, eUrls) = (filterLeft eeUrls, filterRight eeUrls)
    dUrls <- holdDyn [] eUrls

    welcomeWindow welcomeWindowTransferStorageKey welcomeTransfer
    dDepositBalance <- holdUniqDyn $ negate . getDeposit <$> dCoins
    containerApp "" $
        transactionBalanceWidget (Formula 0 0 0 0 0 0) Nothing " (to Wallet)"
    let formula = Formula dDepositBalance 0 0 0 (getCoinNumber <$> dCoins) 0
    containerApp "" $
        transactionBalanceWidget formula (Just TransferMode) " (to Ledger)"
    (dCoins, eSendToLedger, eAddr, dTokensV3) <- containerApp "" $ divClass "app-columns w-row" $ mdo
        dImportedSecrets <- foldDyn (++) [] eImportSecret
        let dTokenCache = nubWith tcAssetName <$> zipDynWith (++) dTokenCacheOld (map coinV3 <$> dImportedSecrets)

        -- Save begin
        dTokenCacheUpdated <-
            handleUnsavedTokens dCloudOn dmKey dTokenCache eWasMigration
        -- Save end

        -- save unique changes only
        dTokensUpdatedUniq <- holdUniqDyn dTokenCacheUpdated
        saveCacheLocally mpass "trasferTab" dTokensUpdatedUniq

        (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
            dCTB <- divClassId "" "welcome-coins-transfer" $ do
                mainWindowColumnHeader "Coins in the Wallet"
                dyn_ $ fmap noCoinsFoundWidget dSecretsInTheWallet
                coinBurnCollectionWidget dSecretsInTheWallet
            (eImport, eExport) <-
                divClass "app-columns w-row" $
                    (,) <$> menuButton "Import" <*> menuButton "Export"
            exportWindow eExport dCTB (map tcSecret <$> dTokenCache)
            (eIS, eISAll) <- importWindow eImport
            return (dCTB, leftmost [eIS, eISAll])
        divClassId "app-CoinColumnRight w-col w-col-6" "welcome-transfer-btns" $ do
            eWallet <-
                sendButton
                    ( zipDynWith
                        (&&)
                        (fmap (not . null) dCoinsToBurn)
                        (fmap (not . isStatusWantBlockButtons) dStatus)
                    )
                    ""
                    " Send to Wallet"
            eLedger <-
                sendButton
                    ( zipDynWith
                        (&&)
                        (fmap (not . null) dCoinsToBurn)
                        (fmap (not . isStatusWantBlockButtons) dStatus)
                    )
                    "margin-top: 20px"
                    " Send to Ledger"
            eWalletOk <- sendToWalletWindow eWallet dCoinsToBurn
            (eAddrOk, _) <- inputAddressWindow eWalletOk
            return (dCoinsToBurn, eLedger, eAddrOk, dTokenCacheUpdated)
    dAddr <- holdDyn Nothing (Just <$> eAddr)
    dWalletSignature <- elementResultJS "walletSignatureElement" decodeWitness
    (dAssetNamesInTheWallet, eStatusUpdate1, _) <-
        encoinsTxTransferMode
            dWallet
            dCoins
            dSecretsInTheWallet
            dAddr
            (void eAddr)
            dWalletSignature
            dUrls

    let dSecretsInTheWallet =
            zipDynWith
                filterByKnownCoinNames
                dAssetNamesInTheWallet
                dTokensV3
    (_, eStatusUpdate2, _) <-
        encoinsTxTransferMode
            dWallet
            dCoins
            dSecretsInTheWallet
            (pure Nothing)
            eSendToLedger
            dWalletSignature
            dUrls

    eWalletError <- walletError
    let eStatus =
            leftmost
                [eWalletError, eStatusUpdate1, eStatusUpdate2, NoRelay <$ eUrlError]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Transfer mode" eStatus
    pure dTokensV3
    where
        menuButton =
            divClass "w-col w-col-6"
                . divClass "app-ImportExportButton"
                . btn
                    "button-switching flex-center"
                    "margin-top:20px;min-width:unset"
                . text
        sendButton dActive stl =
            divClass "app-SendTransferButton"
                . btn
                    (("button-switching flex-center " <>) . bool "button-disabled" "" <$> dActive)
                    stl
                . text

ledgerTab ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Dynamic t [TokenCacheV3]
    -> Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
ledgerTab mpass dTokenCacheOld dCloudOn dmKey eWasMigration = sectionApp "" "" $ mdo
    eFetchUrls <- newEvent
    eeUrls <- currentRequestWrapper delegateServerUrl eFetchUrls
    let (eUrlError, eUrls) = (filterLeft eeUrls, filterRight eeUrls)
    dUrls <- holdDyn [] eUrls

    welcomeWindow welcomeWindowLedgerStorageKey welcomeLedger
    (dBalance, dFees, dBulletproofParams, bRandomness) <-
        getEnvironment LedgerMode dAddr dToBurn dToMint

    dDepositBalance <-
        holdUniqDyn $
            zipDynWith (-) (getDeposit <$> dToMint) (getDeposit <$> dToBurn)
    dEncoinsDepositBalance <- holdUniqDyn $ zipDynWith (+) dBalance dDepositBalance
    dTotalBalance <- holdUniqDyn $ zipDynWith (+) dEncoinsDepositBalance dFees
    let formula =
            Formula
                (negate <$> dTotalBalance)
                dFees
                (getAda <$> dToBurn)
                (getAda <$> dToMint)
                (getCoinNumber <$> dToBurn)
                (getCoinNumber <$> dToMint)
    containerApp "" $ transactionBalanceWidget formula (Just LedgerMode) ""

    (dToBurn, dToMint, dAddr, eStatusUpdate, dNewTokensV3) <- containerApp "" $
        divClassId "app-columns w-row" "welcome-ledger" $ mdo
            dImportedSecrets <- foldDyn (++) [] eImportSecret
            dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
            let dTokenCache =
                    fmap (nubWith tcAssetName) $
                        zipDynWith (++) dTokenCacheOld $
                            map coinV3 <$> zipDynWith (++) dImportedSecrets dNewSecrets

            -- Save begin
            dTokensUpdated <- handleUnsavedTokens dCloudOn dmKey dTokenCache eWasMigration
            -- Save end

            -- save unique changes only
            dTokensUpdatedUniq <- holdUniqDyn dTokensUpdated
            saveCacheLocally mpass "ledgerTab" dTokensUpdatedUniq

            (dCoinsToBurn, eImportSecret) <- divClass "w-col w-col-6" $ do
                dCTB <- divClassId "" "welcome-ledger-coins" $ do
                    mainWindowColumnHeader "Coins in the Ledger"
                    dSecretsUniq <- holdUniqDyn dSecretsInTheWallet
                    dyn_ $ fmap noCoinsFoundWidget dSecretsUniq
                    coinBurnCollectionWidget dSecretsUniq
                eImp <- divClass "" $ do
                    (eImport, eExport) <-
                        divClass "app-columns w-row" $
                            (,) <$> menuButton "Import" <*> menuButton "Export"
                    exportWindow eExport dCTB (map tcSecret <$> dTokenCache)
                    (eIS, eISAll) <- importWindow eImport
                    return $ leftmost [eIS, eISAll]
                return (dCTB, eImp)
            (eSendStatus, dCoinsToMint, eSend, dChangeAddr) <-
                divClassId "app-CoinColumnRight w-col w-col-6" "welcome-ledger-mint" $ mdo
                    dCoinsToMint' <- divClass "" $ mdo
                        mainWindowColumnHeader "Coins to Mint"
                        dCoinsToMint'' <-
                            coinMintCollectionV3Widget $
                                leftmost
                                    [ AddCoin <$> eNewSecret
                                    , ClearCoins <$ ffilter (== Submitted) eStatusUpdate
                                    , AddCoin <$> eAddChange
                                    ]
                        eNewSecret <- coinNewWidget
                        return dCoinsToMint''
                    (eSendStatus, eSend') <-
                        sendRequestButtonLedger
                            LedgerMode
                            dStatus
                            dCoinsToBurn
                            dCoinsToMint
                            (void $ updated dBalance)
                            dUrls
                    -- NOTE: When we add change both the fees and the deposit payments are changed. Previously the calculations were not correct.
                    let dV = fmap calculateChange dEncoinsDepositBalance
                        eSendZeroBalance = gate ((== 0) <$> current dTotalBalance) eSend'
                        eSendNonZeroBalance = gate ((/= 0) <$> current dTotalBalance) eSend'
                    eAddChange <- coinNewButtonWidget dV never (addChangeButton dTotalBalance)
                    (eAddrOk, _) <- inputAddressWindow eSendNonZeroBalance
                    let eHasChangeAddress = leftmost [eAddrOk, ledgerAddress <$ eSendZeroBalance]
                    dAddr' <- holdDyn emergentChangeAddress eHasChangeAddress
                    -- wait until ChangeAddress is updated
                    eFireSend <- delay 0.1 $ () <$ eHasChangeAddress
                    pure (eSendStatus, dCoinsToMint', eFireSend, dAddr')

            (dAssetNamesInTheWallet, eStatusUpdate) <-
                encoinsTxLedgerMode
                    dBulletproofParams
                    bRandomness
                    dChangeAddr
                    dCoinsToBurn
                    dCoinsToMint
                    eSend
                    dUrls
            let dSecretsInTheWallet =
                    zipDynWith
                        filterByKnownCoinNames
                        dAssetNamesInTheWallet
                        dTokensUpdated

            pure
                ( dCoinsToBurn
                , dCoinsToMint
                , dChangeAddr
                , leftmost [eStatusUpdate, eSendStatus]
                , dTokensUpdated
                )
    eWalletError <- walletError
    let eStatus = leftmost [eStatusUpdate, eWalletError, NoRelay <$ eUrlError]
    dStatus <- holdDyn Ready eStatus
    tellTxStatus "Ledger status" eStatus
    pure dNewTokensV3
    where
        menuButton =
            divClass "w-col w-col-6"
                . divClass "app-ImportExportButton"
                . btn
                    "button-switching flex-center"
                    "margin-top:20px;min-width:unset"
                . text
        calculateChange bal = negate bal - 8
        f v =
            if v < 0
                then "button-switching flex-center"
                else "button-not-selected button-disabled flex-center"
        addChangeButton dBal = btn (f <$> dBal) "margin-top: 10px;" $ text "ADD CHANGE"

saveCacheLocally ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Text
    -> Dynamic t [TokenCacheV3]
    -> m ()
saveCacheLocally mPass name cache = do
    eSave <- saveAppData mPass encoinsV3 $ updated cache
    logEvent (name <> " saved cache") eSave
    pure ()
