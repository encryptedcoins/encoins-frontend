module ENCOINS.App.Widgets.Coin where

import           Control.Monad               (join)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Bool                   (bool)
import           Data.List                   (delete)
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as Text
import           PlutusTx.Builtins           (fromBuiltin)
import           Reflex.Dom
import           System.Random               (randomIO)
import           Text.Hex                    (encodeHex)
import           Text.Read                   (readMaybe)

import           Backend.EncoinsTx           (bulletproofSetup)
import           ENCOINS.App.Widgets.Basic   (checkboxApp)
import           ENCOINS.BaseTypes           (FieldElement)
import           ENCOINS.Bulletproofs        (Secret (..), fromSecret, Secrets)
import           ENCOINS.Crypto.Field        (toFieldElement)
import           ENCOINS.Website.Widgets     (image)
import           JS.App                      (sha2_256)
import           JS.Website                  (logInfo)
import           Widgets.Basic               (elementResultJS)
import           Widgets.Events              (newEvent)
import           Widgets.Utils               (toText)

coinWithName :: MonadWidget t m => Secret -> m (Dynamic t (Secret, Text))
coinWithName s = do
    logInfo "we are in coinWithName"
    let bsHex = encodeHex . fromBuiltin . snd . fromSecret bulletproofSetup $ s
    elemId <-  Text.append "coin-" . toText <$> liftIO (randomIO :: IO FieldElement)
    dFullName <- elementResultJS elemId id
    e <- newEvent
    performEvent_ (flip sha2_256 elemId <$> (bsHex <$ e))
    return $ fmap (s, ) dFullName

-- TODO: fix recalculation on addition/deletion of secrets
coinCollectionWithNames :: MonadWidget t m => Dynamic t Secrets -> m (Dynamic t [(Secret, Text)])
coinCollectionWithNames dSecrets = do
    eCoinWithNameWidgets <- dyn $ fmap (mapM coinWithName) dSecrets
    let eCoinsWithNames = fmap sequenceA eCoinWithNameWidgets
    join <$> holdDyn (pure []) eCoinsWithNames

shortenCoinName :: Text -> Text
shortenCoinName txt = Text.take 3 txt `Text.append` "..." `Text.append` Text.takeEnd 3 txt

coinValue :: Secret -> Text
coinValue = toText . fst . fromSecret bulletproofSetup

filterKnownCoinNames :: [Text] -> [(Secret, Text)] -> [(Secret, Text)]
filterKnownCoinNames knownNames = filter (\(_, name) -> name `elem` knownNames)

-------------------------------------------- Coins in the Wallet ----------------------------------------

coinBurnWidget :: MonadWidget t m => (Secret, Text) -> m (Dynamic t (Maybe Secret))
coinBurnWidget (s, name) = divClass "coin-entry-burn-div" $ do
    dChecked <- checkboxApp
    divClass "app-text-normal" $ text $ shortenCoinName name
    divClass "app-text-semibold" $ text $ coinValue s `Text.append` " ADA"
    image "Key.svg" "" "22px"

    return $ fmap (bool Nothing (Just s)) dChecked

coinBurnCollectionWidget :: MonadWidget t m => Dynamic t [(Secret, Text)] -> m (Dynamic t Secrets)
coinBurnCollectionWidget dSecretsWithNames = do
    eCoinBurnWidgets <- dyn $ fmap (mapM coinBurnWidget) dSecretsWithNames
    let eCoinsToBurn = fmap (fmap catMaybes . sequenceA) eCoinBurnWidgets
    join <$> holdDyn (pure []) eCoinsToBurn

---------------------------------------------- Coins to Mint ---------------------------------------------

coinMintWidget :: MonadWidget t m => (Secret, Text) -> m (Event t Secret)
coinMintWidget (s, name) = divClass "coin-entry-mint-div" $ do
    e <- domEvent Click . fst <$> elClass' "div" "cross-div" blank
    divClass "app-text-normal" $ text $ shortenCoinName name
    divClass "app-text-semibold" $ text $ coinValue s `Text.append` " ADA"
    return $ s <$ e

coinMintCollectionWidget :: MonadWidget t m => Event t Secret -> m (Dynamic t [(Secret, Text)])
coinMintCollectionWidget eAddSecret = mdo
    let f (s, b) lst = bool (s `delete` lst) (lst ++ [s]) b
    dCoinsToMintWithNames <-  foldDyn f [] (leftmost [fmap (, True) eAddSecret, fmap (, False) eRemoveSecret]) >>= coinCollectionWithNames
    performEvent_ (logInfo . toText <$> updated dCoinsToMintWithNames)
    eRemoveSecret <- dyn (fmap (mapM coinMintWidget) dCoinsToMintWithNames) >>= switchHold never . fmap leftmost
    return dCoinsToMintWithNames

----------------------------------------------- New coin input --------------------------------------------

-- TODO: do not allow to input incorrect values
coinNewInputWidget :: MonadWidget t m => Text -> (Text -> a) -> Event t b -> m (Dynamic t a)
coinNewInputWidget placeholder convert eReset = do
  let conf = def { _inputElementConfig_setValue = pure ("" <$ eReset) } &
        (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "maxlength" =: "7"
        <> "type" =: "number" <> "placeholder" =: placeholder))
  t <- inputElement conf
  return $ convert <$> _inputElement_value t

coinNewButtonWidget :: MonadWidget t m => Dynamic t Integer -> m (Event t Secret)
coinNewButtonWidget dV = do
  gamma0 <- liftIO (randomIO :: IO FieldElement)
  eGamma <- performEvent $ liftIO (randomIO :: IO FieldElement) <$ updated dV
  dGamma <- holdDyn gamma0 eGamma
  (e, _) <- elClass' "div" "plus-div" blank
  let bSecret = current $ zipDynWith Secret dGamma (fmap toFieldElement dV)
      maxV    = 2 ^ (20 :: Integer) - 1 :: Integer
      cond v  = 0 <= v && v <= maxV
  return $ tag bSecret $ ffilter cond (tagPromptlyDyn dV $ domEvent Click e)

coinNewWidget :: MonadWidget t m => m (Event t Secret)
coinNewWidget = divClass "coin-new-div" $ mdo
    eNewSecret <- coinNewButtonWidget dV
    dV <- coinNewInputWidget "Enter ADA amount..." (fromMaybe (-1 :: Integer) . readMaybe . unpack) eNewSecret
    divClass "app-text-semibold" $ text "ADA"
    return eNewSecret

