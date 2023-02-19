{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}


module ENCOINS.App.Widgets.ISPO (calculator) where

import           Data.Aeson                        (ToJSON, FromJSON, decode)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.FileEmbed                    (embedFile)
import           Data.List                         (find)
import           Data.Maybe                        (fromJust)
import           Data.Text                         (Text, pack)
import           GHC.Generics                      (Generic)
import           Reflex.Dom

import           ENCOINS.Website.Widgets.Basic     (pClass)

data DelegatorC = DelegatorC { addressC :: Text, rewardC :: Double }
  deriving (Show, Generic, ToJSON, FromJSON)
type DelegatorsC = [DelegatorC]

delegatorList :: DelegatorsC
delegatorList = fromJust (decode $ fromStrict $(embedFile "result/ispo/calculator.json") :: Maybe DelegatorsC)

calculator :: MonadWidget t m => m ()
calculator = divClass "div-calculator w-row" $ do
  let conf = def & (initialAttributes .~ ("class" =: "text-calculator input-calculator"
        <> "placeholder" =: "stake1uyp2whhzvf27x9s7w959tp48nlw2a7rc9k45nrvhjx7sfhqlnlpzc" ))
  t <- divClass "column-calculator w-col w-col-8" $ inputElement conf
  divClass "column-calculator w-col w-col-4" $ pClass "text-calculator" $ do
    text " "
    let f addr = pack $ show $ maybe 0 rewardC $ find (\(DelegatorC a _) -> a == addr) delegatorList
    dynText $ f <$> _inputElement_value t
    text " ENCS"