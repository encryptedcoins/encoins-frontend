module ENCOINS.Common.Widgets.Connect
    ( connectWidget
    ) where

import Data.Text (Text, take, takeEnd)
import Reflex.Dom
import Prelude hiding (take)

import Backend.Wallet (Wallet (..), WalletName (..))
import ENCOINS.Common.Widgets.Basic (btnWithBlock)
import ENCOINS.Common.Widgets.Wallet (walletIcon)

connectText :: Wallet -> Text
connectText w = case w of
    Wallet None _ _ _ _ -> "CONNECT"
    Wallet _ _ addr _ _ -> take 6 addr <> "..." <> takeEnd 6 addr

connectWidget ::
    (MonadWidget t m) =>
    Dynamic t Wallet
    -> Dynamic t Bool
    -> m (Event t ())
connectWidget dWallet dIsBlockedConnect = divClass "menu-item-button-left" $
    btnWithBlock
        "button-switching flex-center common-Connect_Button"
        ""
        dIsBlockedConnect $ do
        dyn_ $ fmap (walletIcon . walletName) dWallet
        dynText $ fmap connectText dWallet
