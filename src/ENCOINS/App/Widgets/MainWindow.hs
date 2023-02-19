module ENCOINS.App.Widgets.MainWindow where

import           Data.Aeson                    (encode, decodeStrict)
import           Data.Bool                     (bool)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.List                     (nub)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Reflex.Dom

import           Backend.EncoinsTx             (encoinsTx)
import           Backend.Status                (Status(..), walletError)
import           Backend.Wallet                (Wallet (..), WalletName (..))
import           ENCOINS.App.Widgets.Basic     (containerApp, sectionApp, elementResultJS)
import           ENCOINS.App.Widgets.Coin      (CoinUpdate (..), coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget,
                                                    coinCollectionWithNames, filterKnownCoinNames, noCoinsFoundWidget)
import           ENCOINS.Bulletproofs          (Secrets, Secret (..))
import           ENCOINS.Common.Widgets.Basic  (btn)
import           ENCOINS.Crypto.Field          (fromFieldElement)
import           JS.Website                    (saveJSON, loadJSON)
import           Widgets.Events                (newEventWithDelay)
import           Widgets.Utils                 (toText)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Secrets -> Dynamic t Secrets -> m ()
transactionBalanceWidget dCoinsToBurn dCoinsToMint =
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        balance = zipDynWith (-) (getBalance dCoinsToBurn) (getBalance dCoinsToMint)
        balanceSign n = bool "-" "+" (n >= 0)
        balanceADA n = balanceSign n <> toText (abs n) <> " ADA"
    in divClass "transaction-balance-div" $ do
        divClass "app-text-semibold" $ text "Transaction balance:"
        divClass "app-text-semibold" $ dynText $ fmap balanceADA balance

mainWindowColumnHeader :: MonadWidget t m => Text -> m ()
mainWindowColumnHeader title =
    divClass "app-column-head-div" $
        divClass "app-text-semibold" $ text title

loadAppData :: MonadWidget t m => m (Dynamic t Secrets)
loadAppData = do
    let elId = "encoins-data"
    e <- newEventWithDelay 0.1
    performEvent_ (loadJSON "encoins" elId <$ e)
    elementResultJS elId (fromMaybe [] . (decodeStrict :: ByteString -> Maybe Secrets) . encodeUtf8)

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
        [e0, e1, e2, e3, e4, e5]
        [cond0, cond1, cond2, cond3, cond4, cond5]
    where
        f e = bool (TxInvalid e) TxValid
        coins = toBurn ++ toMint
        cond0 = s `notElem` [Balancing, Signing, Submitting, Submitted]
        cond1 = walletName /= None
        cond2 = not $ null toMint
        cond3 = length coins >= 2
        cond4 = length coins <= 5
        cond5 = length coins == length (nub coins)
        e0    = "The transaction is being processed."
        e1    = "Connect ENCOINS DApp to a wallet first."
        e2    = "Minting at least one coin is required to preserve privacy."
        e3    = "At least two coins must be included in a transaction to preserve privacy."
        e4    = "At most five coins can be included in a transaction."
        e5    = "A transaction cannot include coin duplicates."

sendRequestButton :: MonadWidget t m => Dynamic t Status -> Dynamic t Wallet -> Dynamic t Secrets -> Dynamic t Secrets -> m (Event t ())
sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint = do
    let dTxValidity = txValidity <$> dStatus <*> dWallet <*> dCoinsToBurn <*> dCoinsToMint
        f v = case v of
            TxValid     -> "button-switching flex-center"
            TxInvalid _ -> "button-not-selected button-disabled flex-center"
        g v = case v of
            TxValid     -> blank
            TxInvalid e -> divClass "div-tooltip div-tooltip-always-visible" $
                divClass "app-text-normal" $ text e
    e <- btn (fmap f dTxValidity) $ dynText "SEND REQUEST"
    dyn_ $ fmap g dTxValidity
    return $ () <$ ffilter (== TxValid) (current dTxValidity `tag` e)

mainWindow :: MonadWidget t m => Dynamic t Wallet -> m ()
mainWindow dWallet = mdo
    sectionApp "" "" $
        containerApp "" $
            divClass "app-top-menu-div" $ do
                divClass "menu-item-button-right" $ do
                    _ <- btn "" $ dynText "Wallet"
                    blank
                divClass "menu-item-button-right" $ do
                    _ <- btn "button-not-selected button-disabled" $ dynText "Ledger"
                    blank
    sectionApp "" "" $ mdo
        containerApp "" $ transactionBalanceWidget dToBurn dToMint
        (dToBurn, dToMint, eStatusUpdate) <- containerApp "" $
            divClass "app-columns w-row" $ mdo
                dOldSecrets <- loadAppData
                dNewSecrets <- foldDyn (++) [] $ tagPromptlyDyn dCoinsToMint eSend
                let dSecrets = zipDynWith (++) dOldSecrets dNewSecrets
                dSecretsWithNames <- coinCollectionWithNames dSecrets
                performEvent_ (saveJSON "encoins" . decodeUtf8 . toStrict . encode . nub <$> updated dSecrets)

                dCoinsToBurn <- divClass "app-column w-col w-col-6" $ do
                    mainWindowColumnHeader "Coins in the Wallet"
                    dyn_ $ fmap noCoinsFoundWidget dSecretsWithNamesInTheWallet
                    coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    dCoinsToMint <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing) eStatusUpdate]
                    eNewSecret   <- coinNewWidget
                    eSend        <- sendRequestButton dStatus dWallet dCoinsToBurn dCoinsToMint
                    return (dCoinsToMint, eSend)
                (dAssetNamesInTheWallet, eStatusUpdate) <- encoinsTx dWallet dCoinsToBurn dCoinsToMint eSend
                let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
                return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
        eWalletError <- walletError
        dStatus <- holdDyn Ready $ leftmost [eStatusUpdate, eWalletError]
        containerApp "" $ divClass "app-text-small" $ dynText $ fmap toText dStatus