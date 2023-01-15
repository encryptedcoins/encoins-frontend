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
import           ENCOINS.App.Widgets.Coin      (coinNewWidget, coinBurnWidget, coinMintWidget)
import           ENCOINS.Bulletproofs          (Secret(..), Secrets)
import           ENCOINS.Crypto.Field          (Field(..))
import           JS.Website                    (saveJSON, loadJSON)
import           Widgets.Basic                 (elementResultJS)
import           Widgets.Events                (newEventWithDelay)

transactionBalanceWidget :: MonadWidget t m => m ()
transactionBalanceWidget =
    divClass "transaction-balance-div" $
        divClass "app-text-semibold" $ text "+ 406 ADA"

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
    let secrets = [Secret (F 78623591232) (F 3), Secret (F 21879124) (F 5)]
    
    dSecrets <- loadAppData
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
    sectionApp "" "" $ do
        containerApp "" transactionBalanceWidget
        containerApp "" $
            divClass "app-columns w-row" $ do
                divClass "app-column w-col w-col-6" $ do
                    mainWindowColumnHeader "Coins in the Wallet"
                    dyn_ $ fmap (mapM_ coinBurnWidget) dSecrets
                divClass "app-column w-col w-col-6" $ do
                    mainWindowColumnHeader "Coins to Mint"
                    mapM_ coinMintWidget secrets
                    _ <- coinNewWidget
                    _ <- btnApp "" "SEND REQUEST"
                    blank
                encoinsTx (pure []) (pure secrets)
