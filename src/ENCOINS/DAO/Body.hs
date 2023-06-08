module ENCOINS.DAO.Body (bodyWidget) where

import           Data.Functor                       (($>))
import           Reflex.Dom

import           Backend.Wallet                     (walletsSupportedInDAO)
import           ENCOINS.App.Widgets.Basic          (waitForScripts)
import           ENCOINS.App.Widgets.ConnectWindow  (connectWindow)
import           ENCOINS.DAO.Polls                  (poll1)
import           ENCOINS.DAO.Widgets.Navbar         (navbarWidget)
import           ENCOINS.DAO.Widgets.PollWidget     (pollWidget)

bodyContentWidget :: MonadWidget t m => m ()
bodyContentWidget = mdo
  eConnectOpen <- navbarWidget dWallet
  dWallet <- connectWindow walletsSupportedInDAO eConnectOpen

  pollWidget poll1 dWallet

  blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = waitForScripts blank $ mdo
  bodyContentWidget

  eJQueryLoaded <- domEvent Load . fst <$> elAttr'"script" ("src" =: "https://d3e54v103j8qbb.cloudfront.net/js/jquery-3.5.1.min.dc5e7f18c8.js?site=63b058a2f897ba2767d5ff1b"
    <> "type" =: "text/javascript" <> "integrity" =: "sha256-9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=" <> "crossorigin" =: "anonymous") blank
  let e = eJQueryLoaded $> elAttr "script" ("src" =: "js/webflow.js" <> "type" =: "text/javascript") blank
  widgetHold_ blank e
  
