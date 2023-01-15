module ENCOINS.App.Widgets.Coin where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Bifunctor              (bimap)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           PlutusTx.Builtins           (fromBuiltin)
import           PlutusTx.Prelude            (zero)
import           Reflex.Dom
import           System.Random               (randomIO)
import           Text.Hex                    (encodeHex)

import           Backend.EncoinsTx           (bulletproofSetup)
import           ENCOINS.BaseTypes           (FieldElement)
import           ENCOINS.Bulletproofs        (Secret (..), fromSecret)
import           ENCOINS.Website.Widgets     (image)
import           JS.App                      (sha2_256)
import           Widgets.Basic               (elementResultJS)
import           Widgets.Events              (newEvent)
import           Widgets.Utils               (toText)

coinInnerWidget :: MonadWidget t m => Secret -> m (Text, Dynamic t Text)
coinInnerWidget s = do
    let (v, bsHex) = bimap toText (encodeHex . fromBuiltin) $ fromSecret bulletproofSetup s
        shortenName txt = Text.take 3 txt `Text.append` "..." `Text.append` Text.takeEnd 3 txt
    elemId <-  Text.append "coin-" . toText <$> liftIO (randomIO :: IO FieldElement)
    dFullName <- elementResultJS elemId id
    e <- newEvent
    performEvent_ (flip sha2_256 elemId <$> (bsHex <$ e))
    return (v, fmap shortenName dFullName)

coinBurnWidget :: MonadWidget t m => Secret -> m (Dynamic t (Maybe Secret))
coinBurnWidget s = divClass "coin-entry-burn-div" $ do
    (v, dName) <- coinInnerWidget s

    divClass "checkbox-div" blank
    divClass "app-text-normal" $ dynText dName
    divClass "app-text-semibold" $ text $ v `Text.append` " ADA"
    image "Key.svg" "" "22px"
    return $ constDyn $ Just s

coinMintWidget :: MonadWidget t m => Secret -> m (Event t Secret)
coinMintWidget s = divClass "coin-entry-mint-div" $ do
    (v, dName) <- coinInnerWidget s

    divClass "cross-div" blank
    divClass "app-text-normal" $ dynText dName
    divClass "app-text-semibold" $ text $ v `Text.append` " ADA"
    return $ s <$ never

coinNewWidget :: MonadWidget t m => m (Event t Secret)
coinNewWidget = divClass "coin-new-div" $ do
    divClass "plus-div" blank
    divClass "coin-new-input-div" $
        divClass "coin-new-input" $ text "Enter ADA Value"
    divClass "app-text-semibold" $ text "ADA"
    return $ Secret zero zero <$ never