{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Coin where

import           Control.Monad                   (join, void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Bool                       (bool)
import           Data.List                       (delete)
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Text                       (Text, unpack)
import qualified Data.Text                       as Text
import           PlutusTx.Builtins               (fromBuiltin)
import           Reflex.Dom
import           System.Random                   (randomIO)
import           Text.Hex                        (encodeHex)
import           Text.Read                       (readMaybe)

import           Backend.Protocol.Setup          (bulletproofSetup,
                                                  encoinsCurrencySymbol)
import           Backend.Protocol.Types          (AssetName (..),
                                                  CoinStatus (..),
                                                  IpfsStatus (..),
                                                  TokenCacheV3 (..))
import           Backend.Protocol.Utility        (secretToHex)
import           Backend.Utility                 (toText)
import           ENCOINS.BaseTypes               (FieldElement)
import           ENCOINS.Bulletproofs            (Secret (..), Secrets,
                                                  fromSecret)
import           ENCOINS.Common.Widgets.Advanced (checkboxButton, copyButton,
                                                  copyEvent, withTooltip)
import           ENCOINS.Common.Widgets.Basic    (image)
import           ENCOINS.Crypto.Field            (toFieldElement)
import           JS.App                          (fingerprintFromAssetName)
import           JS.Website                      (copyText)

data CoinUpdate = AddCoin Secret | RemoveCoin Secret | ClearCoins

-- TODO: simplify here

-- coinWithName :: Secret -> (Secret, Text)
-- coinWithName s = let bsHex = encodeHex . fromBuiltin . snd . fromSecret bulletproofSetup $ s
--                  in (s, bsHex)

-- coinsWithNames :: Secrets -> [(Secret, Text)]
-- coinsWithNames = map coinWithName

-- coinWithName :: Secret -> (Secret, Text)
-- coinWithName s =
--     let bsHex = encodeHex . fromBuiltin . snd . fromSecret bulletproofSetup $ s
--     in (s, bsHex)

-- TODO: fix recalculation on addition/deletion of secrets
-- coinCollectionWithNames :: MonadWidget t m
--   => Dynamic t Secrets
--   -> m (Dynamic t [(Secret, Text)])
-- coinCollectionWithNames dSecrets = do
--     eCoinWithNameWidgets <- dyn $ fmap (mapM (pure . pure . coinWithName)) dSecrets
--     -- mapM coinWithName :: (a -> m b) -> t a -> m (t b)
--     -- mapM coinWithName :: (Secret -> m (Dynamic t (Secret, Text))) -> [Secret] -> m [Dynamic t (Secret, Text)]
--     -- fmap (mapM coinWithName) dSecrets :: Dynamic t (m [Dynamic t (Secret, Text)])
--     -- dyn :: Dynamic t (m a) -> m (Event t a)
--     -- dyn $ fmap (mapM coinWithName) dSecrets :: m (Event t [Dynamic t (Secret, Text)])
--     let eCoinsWithNames = fmap sequenceA eCoinWithNameWidgets
--     -- sequenceA :: t (f a) -> f (t a)
--     -- sequenceA :: [Dynamic t (Secret, Text)] -> Dynamic t [(Secret, Text)]
--     -- fmap sequenceA eCoinWithNameWidgets :: Event t (Dynamic t [(Secret, Text)])
--     join <$> holdDyn (pure []) eCoinsWithNames

coinV3 :: Secret -> TokenCacheV3
coinV3 s =
    let assetName = MkAssetName
          . encodeHex
          . fromBuiltin
          . snd
          . fromSecret bulletproofSetup
          $ s
    in MkTokenCacheV3 assetName s Unpinned Minted
    -- ^ Unpinned and Minted are default parameters

coinCollectionV3 :: MonadWidget t m
  => Dynamic t Secrets
  -> m (Dynamic t [TokenCacheV3])
coinCollectionV3 dSecrets = do
    eCoinV3Widgets <- dyn $ fmap (mapM (pure . pure . coinV3)) dSecrets
    let eCoinsV3 = fmap sequenceA eCoinV3Widgets
    join <$> holdDyn (pure []) eCoinsV3

shortenCoinName :: Text -> Text
shortenCoinName txt = Text.take 4 txt `Text.append` "..." `Text.append` Text.takeEnd 4 txt

coinValue :: Secret -> Text
coinValue = toText . fst . fromSecret bulletproofSetup

filterByKnownCoinNames :: [AssetName] -> [TokenCacheV3] -> [TokenCacheV3]
filterByKnownCoinNames knownNames =
  filter (\(MkTokenCacheV3 name _ _ _) -> name `elem` knownNames)

-------------------------------------------- Coins in the Wallet ----------------------------------------

coinBurnWidget :: MonadWidget t m => TokenCacheV3 -> m (Dynamic t (Maybe Secret))
coinBurnWidget (MkTokenCacheV3 name s _ _) = mdo
    (elTxt, ret) <- elDynAttr "div" (mkAttrs <$> dTooltipVis) $ do
        dChecked <- divClass "" checkboxButton -- withTooltip checkboxButton mempty 0 0 $
            -- divClass "app-text-normal" $ text "Select to burn this coin."
        (txt,_) <- withTooltip
          (elClass' "div" "app-text-normal" $ text $ shortenCoinName $ getAssetName name)
          mempty
          0
          0
          $ do
            elAttr "div" ("class" =: "app-text-normal" <> "style" =: "width: 350px;") $ text "Click to see the full token name."
        divClass "app-text-semibold ada-value-text" $ text $ coinValue s `Text.append` " ADA"
        let keyIcon = do
                e <- image "Key.svg" "" "22px"
                void $ copyEvent e
                performEvent_ (liftIO (copyText secretText) <$ e)
        divClass "key-div" $ withTooltip keyIcon "left: -400px; width: 400px;" 0 0 $ do
            divClass "app-text-semibold" $ text "Minting Key"
            divClass "app-ToolTip_MintingKey" $ do
                e <- copyButton
                performEvent_ (liftIO (copyText secretText) <$ e)
                text $ " " <> secretText
        return (txt, dChecked)
    dTooltipVis <- toggle False (domEvent Click elTxt)
    dyn_ $ bool blank (coinTooltip name) <$> dTooltipVis
    return $ fmap (bool Nothing (Just s)) ret
    where
        mkAttrs = ("class" =: "coin-entry-burn-div" <>) . bool mempty ("style" =: "background:rgb(50 50 50);")
        secretText = secretToHex s

noCoinsFoundWidget :: MonadWidget t m => [a] -> m ()
noCoinsFoundWidget = bool blank (divClass "coin-entry-burn-div-no-coins" $ divClass "app-text-normal" $ text "No coins found.") . null

coinBurnCollectionWidget :: MonadWidget t m
  => Dynamic t [TokenCacheV3]
  -> m (Dynamic t Secrets)
coinBurnCollectionWidget dlToken = do
    eldmToken <- dyn $ fmap (mapM coinBurnWidget) dlToken
    let edlmToken = fmap sequenceA eldmToken
    let edlToken = fmap (fmap catMaybes) edlmToken
    join <$> holdDyn (constDyn []) edlToken

coinTooltip :: MonadWidget t m => AssetName -> m ()
coinTooltip (MkAssetName name) = elAttr "div" ("class" =: "div-tooltip div-tooltip-always-visible" <>
    "style" =: "border-top-left-radius: 0px; border-top-right-radius: 0px") $ do
    divClass "app-text-semibold" $ text "Full token name"
    elAttr "div" ("class" =: "app-text-normal" <> "style" =: "font-size:16px;overflow-wrap: anywhere;") $ do
        eCopy <- copyButton
        performEvent_ (liftIO (copyText name) <$ eCopy)
        text name
    divClass "app-text-semibold" $ text "Asset fingerprint"
    elAttr "div" ("class" =: "app-text-normal" <> "style" =: "font-size:16px;overflow-wrap: anywhere;") $ do
        eCopy <- copyButton
        fp <- fingerprintFromAssetName encoinsCurrencySymbol name
        performEvent_ (liftIO (copyText fp) <$ eCopy)
        text fp
--------------------------------- Coins to Mint -------------------------------

coinV3MintWidget :: MonadWidget t m => TokenCacheV3 -> m (Event t Secret)
coinV3MintWidget (MkTokenCacheV3 name s _ _) = mdo
    (elTxt, ret) <- elDynAttr "div" (mkAttrs <$> dTooltipVis) $ do
        eCross <- domEvent Click . fst <$> elClass' "div" "cross-div" blank
        (txt, _) <- elClass' "div" "app-text-normal" $ text $ shortenCoinName $ getAssetName name
        divClass "app-text-semibold ada-value-text" $ text $ coinValue s `Text.append` " ADA"
        return (txt, eCross)
    dTooltipVis <- toggle False (domEvent Click elTxt)
    dyn_ $ bool blank (coinTooltip name) <$> dTooltipVis
    return $ s <$ ret
    where
        mkAttrs = ("class" =: "coin-entry-mint-div" <>) . bool mempty ("style" =: "background:rgb(50 50 50);")

coinMintCollectionV3Widget :: MonadWidget t m => Event t CoinUpdate -> m (Dynamic t Secrets)
coinMintCollectionV3Widget eCoinUpdate = mdo
    let f (AddCoin s)    lst = lst ++ [s]
        f (RemoveCoin s) lst = s `delete` lst
        f ClearCoins     _   = []
    dCoinsToMintV3 <- foldDyn f []
      (leftmost [eCoinUpdate, fmap RemoveCoin eRemoveSecret]) >>= coinCollectionV3
    eRemoveSecret <- dyn (fmap (mapM coinV3MintWidget) dCoinsToMintV3)
        >>= switchHold never . fmap leftmost
    return $ fmap (map tcSecret) dCoinsToMintV3

--------------------------------- New coin input ------------------------------

-- TODO: do not allow to input incorrect values
coinNewInputWidget :: MonadWidget t m => Text -> (Text -> a) -> Event t b ->
  m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t a)
coinNewInputWidget placeholder convert eReset = do
  let conf = def { _inputElementConfig_setValue = pure ("" <$ eReset) } &
        (initialAttributes .~ ("class" =: "coin-new-input w-input" <> "maxlength" =: "7"
        <> "type" =: "number" <> "placeholder" =: placeholder))
  t <- inputElement conf
  return (t, convert <$> value t)

coinNewButtonWidget :: MonadWidget t m
  => Dynamic t Integer
  -> Event t ()
  -> m (Event t ())
  -> m (Event t Secret)
coinNewButtonWidget dV eEnter widgetNew = do
  gamma0 <- liftIO (randomIO :: IO FieldElement)
  eGamma <- performEvent $ liftIO (randomIO :: IO FieldElement) <$ updated dV
  dGamma <- holdDyn gamma0 eGamma
  eNewClick <- widgetNew
  let bSecret = current $ zipDynWith Secret dGamma (fmap toFieldElement dV)
      maxV    = 2 ^ (20 :: Integer) - 1 :: Integer
      cond v  = 0 <= v && v <= maxV
      eNew    = leftmost [eNewClick, eEnter]
  return $ tag bSecret $ ffilter cond (tagPromptlyDyn dV eNew)

coinNewWidget :: MonadWidget t m => m (Event t Secret)
coinNewWidget = divClass "coin-new-div" $ mdo
    eNewSecret <- coinNewButtonWidget dV (keypress Enter inp) plusButton
    (inp, dV) <- coinNewInputWidget "Enter ADA amount..." (fromMaybe (-1 :: Integer) . readMaybe . unpack) eNewSecret
    divClass "app-text-semibold" $ text "ADA"
    return eNewSecret
  where
    plusButton = do
      (e, _) <- elClass' "div" "plus-div" blank
      return $ domEvent Click e
