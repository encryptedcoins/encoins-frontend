{-# LANGUAGE OverloadedStrings #-}

module I18n.Common where

import I18n.Common.I18n
import Data.Text (Text)

data CommonMessage
    = MenuTestnet
    | MenuMainnet
    | Connect
    | ConnectWalletWindowTitle
    | Disconnect
    | TutorialWindowTitle
    | HowDelegate
    | HowUseWallet
    | HowUseLedger
    | Yes
    | No
    | Or
    | Ok
    | Enter
    | Save
    | Delete
    deriving stock (Eq, Show)

instance HasI18n Locale CommonMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showCommonMessageEn t
    Locale_RU -> showCommonMessageEn t -- TODO: update it

showCommonMessageEn :: CommonMessage -> Text 
showCommonMessageEn = \case 
    MenuTestnet -> "Testnet"
    MenuMainnet -> "Mainnet"
    Connect -> "Connect"
    ConnectWalletWindowTitle -> "Connect Wallet"
    Disconnect -> "Disconnect"
    TutorialWindowTitle -> "Encoins' tutorials"
    HowDelegate -> "How to delegate"
    HowUseWallet -> "How to use wallet mode"
    HowUseLedger -> "How to use ledger mode"
    Yes -> "Yes"
    No -> "No"
    Or -> "Or"
    Ok -> "OK"
    Enter -> "Enter"
    Save -> "Save"
    Delete -> "Delete"

data WelcomeMessage 
  = WM_Disclaimer 
  | WM_Tabs
  | WM_CoinsInWallet
  | WM_CoinsToMint
  | WM_TransactionBalance
  | WM_ImportExportButtons
  | WM_UsefulLinks
  | WM_SendButtons
  | WM_CoinsInLedger
  | WM_LedgerMode
  deriving stock (Eq, Show)

instance HasI18n Locale WelcomeMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showWelcomeMessageEn t
    Locale_RU -> showWelcomeMessageEn t -- TODO: update it

showWelcomeMessageEn :: WelcomeMessage -> Text 
showWelcomeMessageEn = \case 
  WM_Disclaimer -> "Disclaimer"
  WM_Tabs -> "Tabs"
  WM_CoinsInWallet -> "Coins in the Wallet"
  WM_CoinsToMint -> "Coins to mint"
  WM_TransactionBalance -> "Transaction balance"
  WM_ImportExportButtons -> "Import/Export buttons"
  WM_UsefulLinks -> "Useful links"
  WM_SendButtons -> "Send buttons"
  WM_CoinsInLedger -> "Coins in the Ledger"
  WM_LedgerMode -> "Ledger mode"