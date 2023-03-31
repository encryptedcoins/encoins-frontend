{-# LANGUAGE NumericUnderscores #-}

module ENCOINS.App.Widgets.MainWindow where

import           Data.Aeson                       (encode)
import           Data.Bool                        (bool)
import           Data.ByteString.Lazy             (toStrict)
import           Data.List                        (nub)
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Data.Text.Encoding               (decodeUtf8)
import           PlutusTx.Prelude                 (divide)
import           Reflex.Dom
import           Witherable                       (catMaybes)

import           Backend.EncoinsTx                (encoinsTx)
import           Backend.Status                   (Status(..), walletError)
import           Backend.Wallet                   (Wallet (..), WalletName (..))
import           CSL                              (TransactionUnspentOutput(..), amount, coin)
import           ENCOINS.App.Widgets.Basic        (containerApp, sectionApp, loadAppData)
import           ENCOINS.App.Widgets.Coin         (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                    coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget)
import           ENCOINS.App.Widgets.ImportWindow (importWindow)
import           ENCOINS.Bulletproofs             (Secrets, Secret (..))
import           ENCOINS.Common.Widgets.Basic     (btn)
import           ENCOINS.Crypto.Field             (fromFieldElement)
import           JS.Website                       (saveJSON, logInfo)
import           Widgets.Utils                    (toText)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Secrets -> Dynamic t Secrets -> m ()
transactionBalanceWidget dCoinsToBurn dCoinsToMint =
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        balance = zipDynWith (-) (getBalance dCoinsToBurn) (getBalance dCoinsToMint)
        balanceSign n = bool "-" "+" (n >= 0)
        balanceADA n = "Transaction balance: " <> balanceSign n <> toText (abs n) <> " ADA"
        feeADA n = bool blank (divClass "app-text-semibold" $ text $ "Fee: " <> toText (max 3 $ n `divide` 100) <> " ADA") (n > 0)
    in divClass "transaction-balance-div" $ do
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
    e <- btn (fmap f dTxValidity) (fmap h dTxValidity) $ dynText "SEND REQUEST"
    dyn_ $ fmap g dTxValidity
    return $ () <$ ffilter (== TxValid) (current dTxValidity `tag` e)

mainWindow :: MonadWidget t m => Dynamic t Wallet -> m ()
mainWindow dWallet = mdo
    sectionApp "" "" $
        containerApp "" $
            divClass "app-top-menu-div" $ do
                divClass "menu-item-button-right" $ do
                    _ <- btn "" "" $ dynText "Wallet"
                    blank
                divClass "menu-item-button-right" $ do
                    _ <- btn "button-not-selected button-disabled" "" $ dynText "Ledger"
                    blank
                -- e <- divClass "menu-item-button-right" $
                --     btn "button-switching flex-center" "" $ do
                --         -- void $ image "import.svg" "image-button inverted" "30px"
                --         dynText " Import"
                -- importWindow e

    sectionApp "" "" $ mdo
        containerApp "" $ transactionBalanceWidget dToBurn dToMint
        (dToBurn, dToMint, eStatusUpdate, _) <- containerApp "" $
            divClass "app-columns w-row" $ mdo
                dImportedSecrets <- foldDyn (:) [] (catMaybes eImportSecret)
                performEvent_ $ logInfo . toText <$> updated dImportedSecrets
                dOldSecrets <- loadAppData "encoins" id []
                dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
                let dSecrets = fmap nub $ zipDynWith (++) dImportedSecrets $ zipDynWith (++) dOldSecrets dNewSecrets
                dSecretsWithNames <- coinCollectionWithNames dSecrets
                performEvent_ (saveJSON "encoins" . decodeUtf8 . toStrict . encode <$> updated dSecrets)

                (dCoinsToBurn, eImportSecret) <- divClass "app-column w-col w-col-6" $ do
                    mainWindowColumnHeader "Coins in the Wallet"
                    dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                    dCTB <- coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                    e <- divClass "menu-item-button-right" $
                        btn "button-switching flex-center" "margin-top: 20px" $ do
                        -- void $ image "import.svg" "image-button inverted" "30px"
                            dynText " Import"
                    eIS <- importWindow e
                    return (dCTB, eIS)
                (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret   <- coinNewWidget
                    eSend        <- sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint
                    return (dCoinsToMint, eSend)
                (dAssetNamesInTheWallet, eStatusUpdate, dTxId) <- encoinsTx dWallet dCoinsToBurn dCoinsToMint eSend
                let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
                return (dCoinsToBurn, dCoinsToMint, eStatusUpdate, dTxId)
        eWalletError <- walletError
        dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
        containerApp "" $ divClass "app-text-small" $ do
            dynText $ fmap toText dStatus
            -- let f txId s = bool blank (void $ lnk ("https://preprod.cexplorer.io/tx/" <> txId) "" $ divClass "text-footer" $ text txId) (s == Submitted)
            -- dyn_ $ f <$> dTxId <*> dStatus