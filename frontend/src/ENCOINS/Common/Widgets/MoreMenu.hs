{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Common.Widgets.MoreMenu where

import           Backend.Utility                 (space)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (lnk)

import           Control.Monad                   (void)
import           Data.Text                       (Text)
import           Reflex.Dom


data NavMoreMenuClass = NavMoreMenuClass
  { nmmcContainer :: Text
  , nmmcIcon      :: Text
  }

moreMenuWidget :: MonadWidget  t m
  => NavMoreMenuClass
  -> m (Event t ())
moreMenuWidget cls = do
  elMore <- divClass ("menu-item" <> space <> nmmcContainer cls) $
    fmap fst $ elDynClass' "div" (constDyn $ nmmcIcon cls) (pure ())
  pure $ domEvent Click elMore

data WindowMoreMenuClass = WindowMoreMenuClass
  { wmmcWindow    :: Text
  , wmmcContainer :: Text
  , wmmcLink      :: Text
  }

moreMenuWindow :: MonadWidget t m
  => WindowMoreMenuClass
  -> Event t ()
  -> m ()
moreMenuWindow cls eOpen =
    dialogWindow
        True
        eOpen
        never
        (wmmcWindow cls)
        "Encoins' tutorials" $ do
          divClass (wmmcContainer cls) $ do
            let linkCls = wmmcLink cls
            let videoLink ref name = void $ lnk ref linkCls $ text name
            videoLink linkDelegate "How to delegate"
            videoLink linkWallet "How to use wallet mode"
            videoLink linkLedger "How to use ledger mode"

linkDelegate :: Text
linkDelegate = "https://youtu.be/h5lOPwhT8wA"

linkWallet :: Text
linkWallet = "https://youtu.be/gck5r6Q6m7g"

linkLedger :: Text
linkLedger = "https://youtu.be/ldH7IQL9F-k"
