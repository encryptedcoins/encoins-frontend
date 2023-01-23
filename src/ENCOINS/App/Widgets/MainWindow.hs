module ENCOINS.App.Widgets.MainWindow where

import           Data.Aeson                    (encode, decodeStrict)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Reflex.Dom

import           Backend.EncoinsTx             (encoinsTx)
import           ENCOINS.App.Widgets.Basic     (btnApp, containerApp, sectionApp)
import           ENCOINS.App.Widgets.Coin      (coinNewWidget, coinBurnCollectionWidget, coinMintCollectionWidget, coinCollectionWithNames, filterKnownCoinNames)
import           ENCOINS.Bulletproofs          (Secrets, Secret (secretV))
import           JS.Website                    (saveJSON, loadJSON, logInfo)
import           Widgets.Basic                 (elementResultJS)
import           Widgets.Events                (newEventWithDelay)
import Widgets.Utils (toText)
import qualified Data.Text as Text
import ENCOINS.Crypto.Field (fromFieldElement)
import Data.Bool (bool)

transactionBalanceWidget :: MonadWidget t m => Dynamic t Secrets -> Dynamic t Secrets -> m ()
transactionBalanceWidget dCoinsToBurn dCoinsToMint =
    let getBalance = fmap (sum . map (fromFieldElement . secretV))
        balance = zipDynWith (-) (getBalance dCoinsToBurn) (getBalance dCoinsToMint)
        balanceSign n = bool "- " "+ " (n >= 0)
        balanceText n = balanceSign n `Text.append` toText (abs n) `Text.append` " ADA"
    in divClass "transaction-balance-div" $
        divClass "app-text-semibold" $ dynText $ fmap balanceText balance

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

mainWindow :: MonadWidget t m => m ()
mainWindow = do
    -- let secrets = [Secret (F 78623591232) (F 3), Secret (F 21879124) (F 5)]

    dSecrets <- loadAppData
    dSecretsWithNames <- coinCollectionWithNames dSecrets
    performEvent_ (saveJSON "encoins" . decodeUtf8 . toStrict . encode <$> updated dSecrets)

    sectionApp "" "" $
        containerApp "" $
            divClass "app-top-menu-div" $ do
                divClass "menu-item-button-right" $ do
                    _ <- btnApp "" "Wallet"
                    blank
                divClass "menu-item-button-right" $ do
                    _ <- btnApp "button-not-selected" "Ledger"
                    blank
    sectionApp "" "" $ mdo
        containerApp "" $ transactionBalanceWidget dToBurn dToMint
        (dToBurn, dToMint) <- containerApp "" $
            divClass "app-columns w-row" $ mdo
                dCoinsToBurn <- divClass "app-column w-col w-col-6" $ do
                    mainWindowColumnHeader "Coins in the Wallet"
                    coinBurnCollectionWidget dSecretsWithNamesInTheWallet
                (dCoinsToMint, eSend) <- divClass "app-column w-col w-col-6" $ mdo
                    mainWindowColumnHeader "Coins to Mint"
                    d <- coinMintCollectionWidget eNewSecret
                    let d' = fmap (map fst) d
                    eNewSecret <- coinNewWidget
                    e <- btnApp "" "SEND REQUEST"
                    return (d', e)
                dAssetNamesInTheWallet <- encoinsTx dCoinsToBurn dCoinsToMint eSend
                let dSecretsWithNamesInTheWallet = zipDynWith filterKnownCoinNames dAssetNamesInTheWallet dSecretsWithNames
                return (dCoinsToBurn, dCoinsToMint)
        blank
