module ENCOINS.App.Widgets.Coin where

import           Control.Monad                   (join, void)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.Bool                       (bool)
import           Data.List                       (delete)
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Text                       (Text, unpack)
import qualified Data.Text                       as Text
import           PlutusTx.Builtins               (fromBuiltin, toBuiltin)
import           Reflex.Dom
import           System.Random                   (randomIO)
import           Text.Hex                        (encodeHex, decodeHex)
import           Text.Read                       (readMaybe)

import           Backend.EncoinsTx               (bulletproofSetup, encoinsCurrencySymbol)
import           ENCOINS.Common.Widgets.Advanced (checkboxButton, copyButton, withTooltip)
import           ENCOINS.Common.Widgets.Basic    (image)
import           ENCOINS.BaseTypes               (FieldElement)
import           ENCOINS.Bulletproofs            (Secret (..), fromSecret, Secrets)
import           ENCOINS.Crypto.Field            (toFieldElement, fromFieldElement)
import           JS.App                          (fingerprintFromAssetName)
import           JS.Website                      (logInfo, copyText)
import           PlutusTx.Extra.ByteString       (toBytes, byteStringToInteger)
import           Widgets.Utils                   (toText)

data CoinUpdate = AddCoin Secret | RemoveCoin Secret | ClearCoins

coinWithName :: MonadWidget t m => Secret -> m (Dynamic t (Secret, Text))
coinWithName s = do
    let bsHex = encodeHex . fromBuiltin . snd . fromSecret bulletproofSetup $ s
    return $ pure (s, bsHex)

-- TODO: fix recalculation on addition/deletion of secrets
coinCollectionWithNames :: MonadWidget t m => Dynamic t Secrets -> m (Dynamic t [(Secret, Text)])
coinCollectionWithNames dSecrets = do
    eCoinWithNameWidgets <- dyn $ fmap (mapM coinWithName) dSecrets
    let eCoinsWithNames = fmap sequenceA eCoinWithNameWidgets
    join <$> holdDyn (pure []) eCoinsWithNames

shortenCoinName :: Text -> Text
shortenCoinName txt = Text.take 4 txt `Text.append` "..." `Text.append` Text.takeEnd 4 txt

coinValue :: Secret -> Text
coinValue = toText . fst . fromSecret bulletproofSetup

filterKnownCoinNames :: [Text] -> [(Secret, Text)] -> [(Secret, Text)]
filterKnownCoinNames knownNames = filter (\(_, name) -> name `elem` knownNames)

-------------------------------------------- Coins in the Wallet ----------------------------------------

coinBurnWidget :: MonadWidget t m => (Secret, Text) -> m (Dynamic t (Maybe Secret))
coinBurnWidget (s, name) = divClass "coin-entry-burn-div" $ do
    let secretText = secretToHex s
    dChecked <- divClass "" $ do
        elAttr "div" ("class" =: "div-tooltip top-right") $ do
            divClass "app-text-normal" $ text "Select to burn this coin."
        checkboxButton
    withTooltip (divClass "app-text-normal" $ text $ shortenCoinName name)
      "left: 0px;" 0 0 $ do
        divClass "app-text-semibold" $ text "Full token name"
        divClass "app-text-normal" $ do
            e <- copyButton
            performEvent_ (liftIO (copyText name) <$ e)
            text name
        divClass "app-text-semibold" $ text "Asset fingerprint"
        divClass "app-text-normal" $ do
            eCopy <- copyButton
            fp <- fingerprintFromAssetName encoinsCurrencySymbol name
            performEvent_ (liftIO (copyText fp) <$ eCopy)
            text fp
    divClass "app-text-semibold ada-value-text" $ text $ coinValue s `Text.append` " ADA"
    divClass "key-div" $ do
        void $ image "Key.svg" "" "22px"
        elAttr "div" ("class" =: "div-tooltip top-right") $ do
            divClass "app-text-semibold" $ text "Minting Key"
            divClass "app-text-normal" $ do
                e <- copyButton
                performEvent_ (liftIO (copyText secretText) <$ e)
                text $ " " <> secretText

    return $ fmap (bool Nothing (Just s)) dChecked

noCoinsFoundWidget :: MonadWidget t m => [a] -> m ()
noCoinsFoundWidget = bool (return ()) (divClass "coin-entry-burn-div" $ divClass "app-text-normal" $ text "No coins found.") . null

coinBurnCollectionWidget :: MonadWidget t m => Dynamic t [(Secret, Text)] -> m (Dynamic t Secrets)
coinBurnCollectionWidget dSecretsWithNames = do
    eCoinBurnWidgets <- dyn $ fmap (mapM coinBurnWidget) dSecretsWithNames
    let eCoinsToBurn = fmap (fmap catMaybes . sequenceA) eCoinBurnWidgets
    join <$> holdDyn (pure []) eCoinsToBurn

---------------------------------------------- Coins to Mint ---------------------------------------------

coinMintWidget :: MonadWidget t m => (Secret, Text) -> m (Event t Secret)
coinMintWidget (s, name) = divClass "coin-entry-mint-div" $ do
    e <- domEvent Click . fst <$> elClass' "div" "cross-div" blank
    withTooltip (divClass "app-text-normal" $ text $ shortenCoinName name)
      "right: -150px;" 0 0 $ do
        divClass "app-text-semibold" $ text "Full token name"
        divClass "app-text-normal" $ do
            eCopy <- copyButton
            performEvent_ (liftIO (copyText name) <$ eCopy)
            text name
        divClass "app-text-semibold" $ text "Asset fingerprint"
        divClass "app-text-normal" $ do
            eCopy <- copyButton
            fp <- fingerprintFromAssetName encoinsCurrencySymbol name
            performEvent_ (liftIO (copyText fp) <$ eCopy)
            text fp
    divClass "app-text-semibold ada-value-text" $ text $ coinValue s `Text.append` " ADA"
    return $ s <$ e

coinMintCollectionWidget :: MonadWidget t m => Event t CoinUpdate -> m (Dynamic t Secrets)
coinMintCollectionWidget eCoinUpdate = mdo
    let f (AddCoin s)    lst = lst ++ [s]
        f (RemoveCoin s) lst = s `delete` lst
        f ClearCoins     _   = []
    dCoinsToMintWithNames <-  foldDyn f [] (leftmost [eCoinUpdate, fmap RemoveCoin eRemoveSecret]) >>= coinCollectionWithNames
    performEvent_ (logInfo . toText <$> updated dCoinsToMintWithNames)
    eRemoveSecret <- dyn (fmap (mapM coinMintWidget) dCoinsToMintWithNames) >>= switchHold never . fmap leftmost
    return $ fmap (map fst) dCoinsToMintWithNames

----------------------------------------------- New coin input --------------------------------------------

-- TODO: do not allow to input incorrect values
coinNewInputWidget :: MonadWidget t m => Text -> (Text -> a) -> Event t b ->
  m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t a)
coinNewInputWidget placeholder convert eReset = do
  let conf = def { _inputElementConfig_setValue = pure ("" <$ eReset) } &
        (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "maxlength" =: "7"
        <> "type" =: "number" <> "placeholder" =: placeholder))
  t <- inputElement conf
  return (t, convert <$> value t)

coinNewButtonWidget :: MonadWidget t m => Dynamic t Integer -> Event t () -> m (Event t Secret)
coinNewButtonWidget dV eEnter = do
  gamma0 <- liftIO (randomIO :: IO FieldElement)
  eGamma <- performEvent $ liftIO (randomIO :: IO FieldElement) <$ updated dV
  dGamma <- holdDyn gamma0 eGamma
  (e, _) <- elClass' "div" "plus-div" blank
  let bSecret = current $ zipDynWith Secret dGamma (fmap toFieldElement dV)
      maxV    = 2 ^ (20 :: Integer) - 1 :: Integer
      cond v  = 0 <= v && v <= maxV
      eNew    = leftmost [domEvent Click e, eEnter]
  return $ tag bSecret $ ffilter cond (tagPromptlyDyn dV eNew)

coinNewWidget :: MonadWidget t m => m (Event t Secret)
coinNewWidget = divClass "coin-new-div" $ mdo
    eNewSecret <- coinNewButtonWidget dV (keypress Enter inp)
    (inp, dV) <- coinNewInputWidget "Enter ADA amount..."
      (fromMaybe (-1 :: Integer) . readMaybe . unpack) eNewSecret
    divClass "app-text-semibold" $ text "ADA"
    return eNewSecret

----------------------------------------------- Helper functions --------------------------------------------

secretToHex :: Secret -> Text
secretToHex s = encodeHex . fromBuiltin $ toBytes $ gamma * 2^(20 :: Integer) + v
    where gamma = fromFieldElement $ secretGamma s
          v     = fromFieldElement $ secretV s

hexToSecret :: Text -> Maybe Secret
hexToSecret txt = do
    bs <- decodeHex txt
    let n = byteStringToInteger $ toBuiltin bs
        (gamma, v) = n `divMod` (2^(20 :: Integer))
    return $ Secret (toFieldElement gamma) (toFieldElement v)
