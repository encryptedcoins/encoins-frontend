{-# LANGUAGE OverloadedStrings #-}

module I18n.Common where

import I18n.Common.I18n
import Common.Utility (column, space)

import Data.Text (Text)
import qualified Data.Text as T

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
    | Cancel
    | AreYouSure
    deriving stock (Eq, Show)

instance HasI18n Locale CommonMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showCommonMessageEn t
    Locale_RU -> showCommonMessageRu t 

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
    Cancel -> "Cancel"
    AreYouSure -> "Are you sure?"

showCommonMessageRu :: CommonMessage -> Text 
showCommonMessageRu = \case 
    MenuTestnet -> "Testnet"
    MenuMainnet -> "Mainnet"
    Connect -> "Подключить"
    ConnectWalletWindowTitle -> "Подключение кошелька"
    Disconnect -> "Отключить"
    TutorialWindowTitle -> "Инструкции"
    HowDelegate -> "Как делегировать"
    HowUseWallet -> "Как использовать режим кошелька"
    HowUseLedger -> "Как использовать режим смарт-контракта"
    Yes -> "Да"
    No -> "Нет"
    Or -> "Или"
    Ok -> "Ок"
    Enter -> "Ввод"
    Save -> "Сохранить"
    Delete -> "Удалить"
    Cancel -> "Отменить"
    AreYouSure -> "Вы уверены?"

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
    Locale_RU -> showWelcomeMessageEn t

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

data StatusMessage 
  = EmptyStatusMessage
  | SM_WalletNetworkError 
  deriving stock (Eq, Show)

instance HasI18n Locale StatusMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showStatusMessageEn t
    Locale_RU -> showStatusMessageEn t -- TODO: update it

showStatusMessageEn :: StatusMessage -> Text 
showStatusMessageEn = \case 
  EmptyStatusMessage -> T.empty
  SM_WalletNetworkError -> "Unexpected network! Switch connected wallet to mode" <> column <> space