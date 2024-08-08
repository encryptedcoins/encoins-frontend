{-# LANGUAGE OverloadedStrings #-}

module I18n.Common where

import I18n.Common.I18n
import Data.Text (Text)

data CommonMessage
    = MenuTestnet
    | MenuMainnet
    | Connect
    | ConnectWallet
    | Disconnect
    | Tutorial
    | HowDelegate
    | HowUseWallet
    | HowUseLedger
    | Yes
    | No
    | Or
    | Ok
    deriving stock (Eq)

instance HasI18n Locale CommonMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showCommonMessageEn t
    Locale_RU -> showCommonMessageEn t -- TODO: update it

showCommonMessageEn :: CommonMessage -> Text 
showCommonMessageEn = \case 
    MenuTestnet -> "Testnet"
    MenuMainnet -> "Mainnet"
    Connect -> "Connect"
    ConnectWallet -> "Connect Wallet"
    Disconnect -> "Disconnect"
    Tutorial -> "Encoins' tutorial"
    HowDelegate -> "How to delegate"
    HowUseWallet -> "How to use wallet mode"
    HowUseLedger -> "How to use ledger mode"
    Yes -> "Yes"
    No -> "No"
    Or -> "Or"
    Ok -> "OK"
