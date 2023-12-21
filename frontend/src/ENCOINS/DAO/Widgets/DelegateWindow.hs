{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.DAO.Widgets.DelegateWindow
  (
    delegateWindow
  ) where

import           Control.Monad                   (void)
import           Data.Bool                       (bool)
import           Data.Map                        (Map)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex.Dom

import           Backend.Status                  (UrlStatus (..), isNotValidUrl)
import           Backend.Wallet                  (LucidConfig (..), Wallet (..),
                                                  lucidConfigDao, toJS)
import           ENCOINS.App.Widgets.Basic       (containerApp)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (checkUrl, stripHostOrRelay,
                                                  toText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn, btnWithBlock, divClassId)
import           ENCOINS.DAO.Widgets.RelayTable  (fetchDelegatedByAddress,
                                                  fetchRelayTable,
                                                  relayAmountWidget, unStakeUrl)
import qualified JS.DAO                          as JS


delegateWindow :: MonadWidget t m
  => Event t ()
  -> Dynamic t Wallet
  -> Dynamic t (Map Text Text)
  -> m ()
delegateWindow eOpen dWallet dRelayNames = mdo
  eDelay <- delay 0.05 eOpen
  eeRelays <- fetchRelayTable eDelay
  emDelegated <- fetchDelegatedByAddress (walletChangeAddress <$> dWallet) eDelay
  eUrlOk <- dialogWindow
    True
    eOpen
    (leftmost [void eUrlOk])
    delegateWindowStyle
    "Delegate ENCS" $ mdo
      eUrlTable <- relayAmountWidget eeRelays emDelegated dRelayNames
      divClass "dao-DelegateWindow_EnterUrl" $ text "Choose a relay URL above or enter a new one below:"

      dInputText <- inputWidget eOpen
      let eInputText = updated dInputText

      let eNonEmptyUrl = ffilter (not . T.null) eInputText
      let eEmptyUrl = ffilter T.null eInputText
      let eUrlStatus = leftmost
            [
              UrlEmpty   <$ eEmptyUrl
            -- Check url ONLY when it is not empty
            , bool UrlInvalid UrlValid . checkUrl <$> eNonEmptyUrl
            ]

      dIsInvalidUrl <- holdDyn UrlEmpty eUrlStatus
      (eStake, eUnstake) <- stakingButtonWidget dIsInvalidUrl

      let eUrlStake = tagPromptlyDyn dInputText eStake
      let eUrlUnstake = unStakeUrl <$ eUnstake
      let eUrl = stripHostOrRelay <$> leftmost [eUrlTable, eUrlStake, eUrlUnstake]
      let LucidConfig apiKey networkId policyId assetName = lucidConfigDao
      performEvent_ $
        JS.daoDelegateTx apiKey networkId policyId assetName
        <$> attachPromptlyDyn (fmap (toJS . walletName) dWallet) eUrl
      return eUrl
  pure ()

delegateWindowStyle :: Text
delegateWindowStyle = "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px);"

inputWidget :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Text)
inputWidget eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: "w-input"
          <> "style" =: "display: inline-block;"
          <> "placeholder" =: "format of url: http(s)://address.of.relay/"
          )
      & inputElementConfig_setValue .~ ("" <$ eOpen)
    setFocusDelayOnEvent inp eOpen
    return $ value inp

stakingButtonWidget :: MonadWidget t m
  => Dynamic t UrlStatus
  -> m (Event t (), Event t ())
stakingButtonWidget dUrlStatus =
  divClass "dao-DelegateWindow_ButtonStatusContainer" $ do
    -- The Stake button disable with invalid url and performant status.
    eStake <- btnWithBlock
        "button-switching inverted flex-center"
        ""
        (isNotValidUrl <$> dUrlStatus)
        (text "Delegate")
    divClass "menu-item-button-right" $ do
      containerApp ""
        $ divClassId "app-text-small" ""
        $ dynText
        $ toText <$> dUrlStatus
    eUnstake <- btn "button-switching inverted flex-center" "" (text "Unstake")
    pure (eStake, eUnstake)
