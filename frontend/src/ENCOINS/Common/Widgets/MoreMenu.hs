{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.Common.Widgets.MoreMenu where

import Common.Events
import Common.Utility (space)
import ENCOINS.Common.Widgets.Advanced (dialogWindow)
import ENCOINS.Common.Widgets.Basic (lnk)
import qualified I18n.Common as I18n
import I18n.I18n (App)
import qualified I18n.I18n as I18n

import Control.Monad (void)
import Data.Text (Text)
import Reflex.Dom

data NavMoreMenuClass = NavMoreMenuClass
    { nmmcContainer :: Text
    , nmmcIcon :: Text
    }

viewMoreMenu ::
    (MonadWidget t m) =>
    NavMoreMenuClass
    -> m (Event t ())
viewMoreMenu cls = do
    elMore <-
        divClass ("menu-item" <> space <> nmmcContainer cls) $
            fmap fst $
                elDynClass' "div" (constDyn $ nmmcIcon cls) (pure ())
    pure $ domEvent Click elMore

data WindowMoreMenuClass = WindowMoreMenuClass
    { wmmcWindow :: Text
    , wmmcContainer :: Text
    , wmmcLink :: Text
    }

moreMenuWindow ::
    (App t m) =>
    WindowMoreMenuClass
    -> Event t ()
    -> m ()
moreMenuWindow cls eOpen =
    dialogWindow
        True
        eOpen
        never
        (wmmcWindow cls)
        (I18n.CommonTerm I18n.TutorialWindowTitle)
        $ do
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
