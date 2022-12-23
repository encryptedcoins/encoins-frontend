module Widgets.App where

import           Control.Monad.IO.Class  (MonadIO(..))
import           Data.Bool               (bool)
import           Data.Text               (Text, pack)
import           PlutusTx.Builtins       (emptyByteString)
import           PlutusTx.Prelude        (zero)
import           Reflex.Dom

import           ENCOINS.BaseTypes       (groupGenerator)
import           ENCOINS.Bulletproofs    (Proof(..))
import           JS.App                  (encoinsTx)
import           JS.WebPage              (logInfo)
import           Reflex.ScriptDependent  (widgetHoldUntilDefined)
import           Widgets.Basic           (elementResultJS)

appWidget :: MonadWidget t m => m ()
appWidget = divClass "" $ do
    mainForm

columnClass :: Text
columnClass = ""

mainForm :: MonadWidget t m => m ()
mainForm = divClass columnClass . divClass "" $ mdo
    ePb <- getPostBuild
    eEncoinsLoaded <- updated <$> widgetHoldUntilDefined "walletEnable" ("js/ENCOINS.js" <$ ePb) blank blank

    -- TODO: remove this (test code)
    _ <- elementResultJS "testResultElement" id
    let g = groupGenerator
        f = zero
        red = ("", (1, []), Proof g g g g f f [] [] f, emptyByteString)
    logInfo $ pack $ show red
    performEvent_ $ liftIO . encoinsTx "nami" red <$> ("testResultElement" <$ eEncoinsLoaded)

    -- -- TODO: implement wallet switcher
    -- dWalletName <- holdDyn "nami" never
    -- -- TODO: remove this (test code)
    -- performEvent_ $ logInfo <$> updated dWalletName

    -- let parseConnected b = bool False True (b == "true")
    -- dWalletConnected <- fmap (fmap parseConnected . value) $ inputElement $ def
    --   & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId)
    --   & inputElementConfig_initialValue .~ "false"
    -- -- TODO: remove this (test code)
    -- performEvent_ $ logInfo . pack . show <$> updated dWalletConnected

    blank
  where
    elemId = "input-is-enabled"