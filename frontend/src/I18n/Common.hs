{-# LANGUAGE OverloadedStrings #-}

module I18n.Common where

import I18n.Common.I18n
import Common.Utility (column, space, toText)

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
  = SM_Empty
  | SM_WalletNetworkError 
  | SM_WalletMode
  | SM_TransactionSuccess
  | SM_Constructing
  | SM_Signing
  | SM_Submitting
  | SM_Submitted
  | SM_NoRelay
  | SM_TransferMode
  | SM_LedgerMode
  | SM_InvalidChangeAddress
  | SM_Migration
  | SM_SuccessMigration
  | SM_MigrationUpdate
  | SM_Cloud
  | SM_RestoreFailed
  | SM_RestoreSuccess Int
  | SM_Delegate
  | SM_Vote
  | SM_UrlEmpty
  | SM_UrlInvalid
  | SM_UrlValid
  | SM_Custom Text
  deriving stock (Eq, Show)

instance HasI18n Locale StatusMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showStatusMessageEn t
    Locale_RU -> showStatusMessageRu t 

showStatusMessageEn :: StatusMessage -> Text 
showStatusMessageEn = \case 
  SM_Empty -> T.empty
  SM_WalletNetworkError -> "Unexpected network! Switch connected wallet to mode" <> column <> space
  SM_WalletMode -> "Wallet mode" <> column <> space
  SM_TransactionSuccess -> "Transaction finished successfully"
  SM_Constructing -> "Constructing the transaction..."
  SM_Signing -> "Please sign the transaction."
  SM_Submitting -> "Submitting..."
  SM_Submitted -> "Submitted. Pending the confirmation..."
  SM_NoRelay -> "All available relays are down!"
  SM_TransferMode -> "Transfer mode" <> column <> space
  SM_LedgerMode -> "Ledger mode" <> column <> space
  SM_InvalidChangeAddress -> "ChangeAddress is invalid"
  SM_Migration -> "Migration" <> column <> space
  SM_SuccessMigration -> "Local cache was updated to the last version"
  SM_MigrationUpdate -> "Cache structure is updating. Please wait."
  SM_Cloud -> "Cloud" <> column <> space
  SM_RestoreFailed -> "Restoring tokens failed"
  SM_RestoreSuccess n -> "Restored " <> toText n <> " tokens (with duplicates)"
  SM_Delegate -> "Delegate" <> column <> space
  SM_Vote -> "Vote" <> column <> space
  SM_UrlEmpty -> "URL is empty"
  SM_UrlInvalid -> "Invalid URL format"
  SM_UrlValid -> "Valid URL"
  SM_Custom t -> t

showStatusMessageRu :: StatusMessage -> Text 
showStatusMessageRu = \case 
  SM_Empty -> T.empty
  SM_WalletNetworkError -> "Не та сеть! Переключите подключенный кошелек в режим" <> column <> space
  SM_WalletMode -> "Режим кошелька" <> column <> space
  SM_TransactionSuccess -> "Транзакция успешно завершилась"
  SM_Constructing -> "Создание транзакции..."
  SM_Signing -> "Подпишите транзакцию, пожалуйста."
  SM_Submitting -> "Отправка..."
  SM_Submitted -> "Отправлено. Ожидание подтверждения..."
  SM_NoRelay -> "Нет доступных релеев!"
  SM_TransferMode -> "Режим передачи" <> column <> space
  SM_LedgerMode -> "Режим контракта" <> column <> space
  SM_InvalidChangeAddress -> "ChangeAddress неверный"
  SM_Migration -> "Миграция" <> column <> space
  SM_SuccessMigration -> "Локальный кэш обновлен до последней версии"
  SM_MigrationUpdate -> "Обновляется локальный кэш. Пожалуйста подождите."
  SM_Cloud -> "Облако" <> column <> space
  SM_RestoreFailed -> "Не удалось восстановить токены"
  SM_RestoreSuccess n -> "Восстановлено" <> space <> toText n <> " токенов (с дубликатами)"
  SM_Delegate -> "Делегация" <> column <> space
  SM_Vote -> "Голосование" <> column <> space
  SM_UrlEmpty -> "URL пустой"
  SM_UrlInvalid -> "Некорректный URL формат"
  SM_UrlValid -> "Корректный URL"
  SM_Custom t -> t