{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.MoreMenuWindow where

import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (lnk)

import           Control.Monad                   (void)
import           Data.Text                       (Text)
import           Reflex.Dom


moreMenuWindow :: MonadWidget t m
  => Event t ()
  -> m ()
moreMenuWindow eOpen =
    dialogWindow
        True
        eOpen
        never
        "app-MoreMenu_Window"
        "Encoins' tutorials" $ do
          divClass "app-MoreMenu_LinkContainer" $ do
            void $ lnk linkDelegate "app-MoreMenu_Link" $ text "How to delegate"
            void $ lnk linkWallet "app-MoreMenu_Link" $ text "How to use wallet mode"
            void $ lnk linkLedger "app-MoreMenu_Link" $ text "How to use ledger mode"

linkDelegate :: Text
linkDelegate = "https://youtu.be/h5lOPwhT8wA"

linkWallet :: Text
linkWallet = "https://youtu.be/gck5r6Q6m7g"

linkLedger :: Text
linkLedger = "https://youtu.be/ldH7IQL9F-k"
