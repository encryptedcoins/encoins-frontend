{-# LANGUAGE OverloadedStrings #-}

module I18n.App where

import I18n.Common.I18n
import Data.Text (Text)

data AppMessage
    = PassProtect
    | PassCurrent
    | PassEnter
    | PassRepeat
    | PassSave
    | PassReset
    | PassClean
    | CleanCacheTitle
    | CleanCacheText
    | CleanCacheCancel
    | TabWallet
    | TabTransfer
    | TabLedger
    | Balance
    | BalanceFormula
    | BalanceBurnAda
    | BalanceMintAda
    | BalanceFee
    | BalanceCommission
    | BalanceToWallet
    | BalanceToLedger
    | BalanceNumberEncoins
    | BalanceDeposit
    | BalanceBurnEncoins
    | BalanceMintEncoins
    | CoinsInWallet
    | CoinsMint
    | CoinsInLedger
    | MintingKey
    | TokenName
    | Asset
    | EnterAdaAmount
    | ButtonSendRequest
    | ButtonSentWallet
    | ButtonSendLedger
    | ButtonAddChange
    | Import
    | ImportTitle
    | ImportCoin
    | ImportCoins
    | ImportFile
    | ImportNoFile
    | Export
    | ExportTitle
    | ExportName
    | ExportSave
    | ExportAll
    deriving stock (Eq)

instance HasI18n Locale AppMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showAppMessageEn t
    Locale_RU -> showAppMessageRu t

showAppMessageEn :: AppMessage -> Text 
showAppMessageEn = \case 
    PassProtect -> "Protect cache of Encoins app"
    PassCurrent -> "Current password"
    PassEnter -> "Enter password"
    PassRepeat -> "Repeat password"
    PassSave -> "Save"
    PassReset -> "Reset password"
    PassClean -> "Clean cache"
    CleanCacheTitle -> "Clean cache"
    CleanCacheText -> "This action will reset password and clean cache (remove known coins)!"
    CleanCacheCancel -> "Cancel"
    TabWallet -> "Wallet"
    TabTransfer -> "Transfer"
    TabLedger -> "Ledger"
    Balance -> "Transaction balance"
    BalanceFormula -> "Balance formule"
    BalanceBurnAda -> "sum of Ada in the encoins being burned"
    BalanceMintAda -> "sum of Ada in the encoins being minted"
    BalanceFee -> "fee"
    BalanceCommission -> "commission of the relay"
    BalanceToWallet -> "to Wallet"
    BalanceToLedger -> "to Ledger"
    BalanceNumberEncoins -> "number of the encoins being transferred"
    BalanceDeposit -> "returnable deposit for placing your encoins into the ledger (4 Ada)"
    BalanceBurnEncoins -> "number of the encoins being burned"
    BalanceMintEncoins -> "number of the encoins being minted"
    CoinsInWallet -> "Coins in the Wallet"
    CoinsMint -> "Coins to Mint"
    CoinsInLedger -> "Coins in the Ledger"
    MintingKey -> "Minting Key"
    TokenName -> "Full token name"
    Asset -> "Asset fingerprint"
    EnterAdaAmount -> "Enter Ada amount"
    ButtonSendRequest -> "Send Request"
    ButtonSentWallet -> "Send to Wallet"
    ButtonSendLedger -> "Send to Ledger"
    ButtonAddChange -> "Add change"
    Import -> "Import"
    ImportTitle -> "Import New Encoins"
    ImportCoin -> "Enter the minting key to import a new coin"
    ImportCoins -> "Choose the file to import coins"
    ImportFile -> "Choose file"
    ImportNoFile -> "No file choosen"
    Export -> "Export"
    ExportTitle -> "Export Encoins"
    ExportName -> "Enter file name"
    ExportSave -> "Save Selected"
    ExportAll -> "Save all"

showAppMessageRu :: AppMessage -> Text 
showAppMessageRu = \case 
    PassProtect -> "Защитить кэш Encoins приложения"
    PassCurrent -> "Текущий пароль"
    PassEnter -> "Ввести пароль"
    PassRepeat -> "Повторить пароль"
    PassSave -> "Сохранить"
    PassReset -> "Сбросить пароль"
    PassClean -> "Отчистить кэш"
    CleanCacheTitle -> "Отчистка кэша"
    CleanCacheText -> "Это действие сбросит пароль и отчистить кэш (все известные токены удалятся!"
    CleanCacheCancel -> "Отмена"
    TabWallet -> "Кошелек"
    TabTransfer -> "Отправка"
    TabLedger -> "Смарт-контракт"
    Balance -> "Баланс транзакции"
    BalanceFormula -> "Формула расчета"
    BalanceBurnAda -> "сумма Ada в токенах для сжигания"
    BalanceMintAda -> "сумма Ada в токенах для чеканки"
    BalanceFee -> "комиссия"
    BalanceCommission -> "комиссия релея"
    BalanceToWallet -> "в Кошелек"
    BalanceToLedger -> "на Смарт-контракт"
    BalanceNumberEncoins -> "количество токенов для отправки"
    BalanceDeposit -> "возвращаемый депозит для размещения ваших токенов на Смарт-контракте (4 Ada)"
    BalanceBurnEncoins -> "число токенов для сжигания"
    BalanceMintEncoins -> "число токенов для чеканки"
    CoinsInWallet -> "Токены в Кошельке"
    CoinsMint -> "Токены для чеканки"
    CoinsInLedger -> "Токены на Смарт-контракте"
    MintingKey -> "Minting Key"
    TokenName -> "Full token name"
    Asset -> "Asset fingerprint"
    EnterAdaAmount -> "Введите сумму в Ada"
    ButtonSendRequest -> "Послать запрос"
    ButtonSentWallet -> "Послать на Кошелек"
    ButtonSendLedger -> "Послать на Смарт-контракт"
    ButtonAddChange -> "Добавить сдачу"
    Import -> "Импорт"
    ImportTitle -> "Импорт новых токенов"
    ImportCoin -> "Введите minting key для импорта нового токена"
    ImportCoins -> "Выберите файл с токенами"
    ImportFile -> "Выбрать файл"
    ImportNoFile -> "Файл не выбран"
    Export -> "Экспорт"
    ExportTitle -> "Экспорт токены"
    ExportName -> "Введите имя файла"
    ExportSave -> "Сохранить выделенное"
    ExportAll -> "Сохранить все"

