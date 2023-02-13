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
import           Backend.Status                (Status(..))
import           Backend.Wallet                (Wallet)
import           ENCOINS.App.Widgets.Basic     (btnApp, containerApp, sectionApp)
import           ENCOINS.App.Widgets.Coin      (coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget, coinCollectionWithNames, filterKnownCoinNames, CoinUpdate (..))
import           ENCOINS.Bulletproofs          (Secrets, Secret (..))
import           ENCOINS.Crypto.Field          (fromFieldElement)
import           JS.Website                    (saveJSON, loadJSON)
import           Widgets.Basic                 (elementResultJS)
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

mainWindow :: MonadWidget t m => Dynamic t Wallet -> m ()
mainWindow dWallet = mdo
    sectionApp "" "" $
        containerApp "" $
            divClass "app-top-menu-div" $ do
                divClass "menu-item-button-right" $ do
                    _ <- btnApp "" $ dynText "Wallet"
                    blank
                divClass "menu-item-button-right" $ do
                    _ <- btnApp "button-not-selected button-disabled" $ dynText "Ledger"
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
                    coins <- coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                    divClass "coin-entry-burn-div" $ divClass "app-text-normal" $ dynText $ fmap (bool "" "No coins found." . null) coins
                    return coins
                (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    d <- coinMintCollectionWidget $ leftmost [fmap AddCoin eNewSecret, ClearCoins <$ ffilter (== Balancing)  eStatusUpdate]
                    let d' = fmap (map fst) d
                    eNewSecret <- coinNewWidget
                    e <- btnApp "button-switching" $ dynText "SEND REQUEST"
                    return (d', e)
                (dAssetNamesInTheWallet, eStatusUpdate) <- encoinsTx dWallet dCoinsToBurn dCoinsToMint eSend
                let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
                return (dCoinsToBurn, dCoinsToMint, eStatusUpdate)
        dStatusText <- holdDyn "" $ fmap toText eStatusUpdate
        containerApp "" $ divClass "app-text-small" $ dynText dStatusText