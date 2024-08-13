{-# LANGUAGE OverloadedStrings #-}

module I18n.App where

import I18n.Common.I18n
import Data.Text (Text)

data AppMessage
    = PassWindowProtect
    | PassCurrent
    | PassEnter
    | PassRepeat
    | PassButtonReset
    | PassButtonClean
    | PassNotMatch
    | PassIncorrect
    | PassInvalidNotAll
    | PassInvalidLess10
    | PassInvalidNoUpper
    | PassInvalidNoLower
    | PassInvalidNoNumber
    | PassInvalidNoSpecial
    | PassEntry
    | PassSaved
    | PassCleared
    | CleanCacheWindowTitle
    | CleanCacheText
    | CleanCacheButtonClean
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
    | ImportWindowTitle
    | ImportCoin
    | ImportCoins
    | ImportFile
    | ImportNoFile
    | Export
    | ExportWindowTitle
    | ExportName
    | ExportSave
    | ExportAll
    | CloudWindowTitle
    | CloudToggleDescription
    | CloudStatusTitle
    | CloudStatusDescriptionSuccess
    | CloudKeyTitle 
    | CloudKeyTip
    | CloudInputPlaceholder
    | CloudButtonGenerate 
    | CloudButtonSignKey
    | CloudButtonEnterTip
    | CloudButtonGenerateTip
    | CloudButtonSignKeyTip
    | CloudButtonDeleteTip
    | CloudButtonTipDefault
    | CloudRestoreTitle 
    | CloudButtonRestore
    | CloudDeleteWindowTitle
    | CloudDeleteWindowContent
    | TransferCopySendKeys
    deriving stock (Eq, Show)

instance HasI18n Locale AppMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showAppMessageEn t
    Locale_RU -> showAppMessageRu t

showAppMessageEn :: AppMessage -> Text 
showAppMessageEn = \case 
    PassWindowProtect -> "Protect cache of Encoins app"
    PassCurrent -> "Current password:"
    PassEnter -> "Enter password:"
    PassRepeat -> "Repeat password:"
    PassButtonReset -> "Reset password"
    PassButtonClean -> "Clean cache"
    PassNotMatch -> "Password doesn't match"
    PassIncorrect -> "Incorrect password"
    PassInvalidNotAll -> "Password must consist of \
            \uppercase and lowercase letters, numbers, and special characters"
    PassInvalidLess10 -> "Password must be at least 10 characters long"
    PassInvalidNoUpper -> "Password must contain at least one upper-case letter"
    PassInvalidNoLower -> "Password must contain at least one lower-case letter"
    PassInvalidNoNumber -> "Password must contain at least one number"
    PassInvalidNoSpecial -> "Password must contain at least one special character"
    PassEntry -> "Password for the cache of Encoins app"
    PassSaved -> "Password saved!"
    PassCleared -> "Password cleared!"
    CleanCacheWindowTitle -> "Clean cache"
    CleanCacheText -> "This action will reset password and clean cache (remove known coins)!"
    CleanCacheButtonClean -> "Clean"
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
    ImportWindowTitle -> "Import New Encoins"
    ImportCoin -> "Enter the minting key to import a new coin"
    ImportCoins -> "Choose the file to import coins"
    ImportFile -> "Choose file"
    ImportNoFile -> "No file chosen"
    Export -> "Export"
    ExportWindowTitle -> "Export Encoins"
    ExportName -> "Enter file name"
    ExportSave -> "Save Selected"
    ExportAll -> "Save all"
    CloudWindowTitle -> "Encoins Cloud Backup"
    CloudToggleDescription -> "Save encoins on cloud"
    CloudStatusTitle -> "Cloud synchronization status"
    CloudStatusDescriptionSuccess -> "The synchronization is completed successfully."
    CloudKeyTitle -> "Your AES key for restoring encoins. Save it to a file and keep it secure!" 
    CloudKeyTip -> "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    CloudInputPlaceholder -> "cloud key should be exactly 64 hexadecimal digits"
    CloudButtonGenerate -> "Generate" 
    CloudButtonSignKey -> "SignKey"
    CloudButtonEnterTip -> "Button 'Enter' confirmes manually input key."
    CloudButtonGenerateTip -> "Button 'Generate' generates random cloud key."
    CloudButtonSignKeyTip -> "Button 'SignKey' makes key basing on the sign of connected wallet."
    CloudButtonDeleteTip -> "Button 'Delete' removes currently set key."
    CloudButtonTipDefault -> "To see more details, hover over the active button."
    CloudRestoreTitle -> "Restore all unburned encoins from cloud with your current key" 
    CloudButtonRestore -> "Restore"
    CloudDeleteWindowTitle -> "Delete Cloud Key"
    CloudDeleteWindowContent -> "This action will remove cloud key from the cache! If you won't remember the key you can't recover encoins from remote server! Are you sure?"
    TransferCopySendKeys -> "Copy and send these keys to your recepient off-chain:"

showAppMessageRu :: AppMessage -> Text 
showAppMessageRu = \case 
    PassWindowProtect -> "Защита кэша Encoins приложения"
    PassCurrent -> "Текущий пароль"
    PassEnter -> "Введите пароль:"
    PassRepeat -> "Повторите пароль:"
    PassButtonReset -> "Сбросить пароль"
    PassButtonClean -> "Отчистить кэш"
    PassNotMatch -> "Пароль не подходит"
    PassIncorrect -> "Неправильный пароль"
    PassInvalidNotAll -> "Пароль должен состоять из \
            \заглавных и строчных букв, чисел, и специальных символов"
    PassInvalidLess10 -> "Пароль должен быть длинной не менее 10 символов"
    PassInvalidNoUpper -> "Пароль должен содержать по крайней мере одну заглавную букву"
    PassInvalidNoLower -> "Пароль должен содержать по крайней мере одну строчную букву"
    PassInvalidNoNumber -> "Пароль должен содержать по крайней мере одну цифру"
    PassInvalidNoSpecial -> "Пароль должен содержать по крайней мере один специальный символ"  
    PassEntry -> "Пароль для кэша Encoins приложения в браузере"
    PassSaved -> "Пароль сохранен!"
    PassCleared -> "Пароль удален!"
    CleanCacheWindowTitle -> "Отчистка кэша"
    CleanCacheText -> "Это действие сбросит пароль и отчистить кэш (все известные токены удалятся!"
    CleanCacheButtonClean -> "Отчистить"
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
    MintingKey -> "Ключ выпуска"
    TokenName -> "Полное имя токена"
    Asset -> "Asset fingerprint"
    EnterAdaAmount -> "Введите сумму в Ada"
    ButtonSendRequest -> "Послать запрос"
    ButtonSentWallet -> "Послать на Кошелек"
    ButtonSendLedger -> "Послать на Смарт-контракт"
    ButtonAddChange -> "Добавить сдачу"
    Import -> "Импорт"
    ImportWindowTitle -> "Импорт новых токенов"
    ImportCoin -> "Введите minting key для импорта нового токена"
    ImportCoins -> "Выберите файл с токенами"
    ImportFile -> "Выбрать файл"
    ImportNoFile -> "Файл не выбран"
    Export -> "Экспорт"
    ExportWindowTitle -> "Экспорт токены"
    ExportName -> "Введите имя файла"
    ExportSave -> "Сохранить выделенное"
    ExportAll -> "Сохранить все"
    CloudWindowTitle -> "Резервное копирование энкойны в облако"
    CloudToggleDescription -> "Сохранить энкойны в облако"
    CloudStatusTitle -> "Статус синхронизации с облаком"
    CloudStatusDescriptionSuccess -> "The synchronization is completed successfully."
    CloudKeyTitle -> "Your AES key for restoring encoins. Save it to a file and keep it secure!" 
    CloudKeyTip -> "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    CloudInputPlaceholder -> "cloud key should be exactly 64 hexadecimal digits"
    CloudButtonGenerate -> "Generate" 
    CloudButtonSignKey -> "SignKey"
    CloudButtonEnterTip -> "Button 'Enter' confirmes manually input key."
    CloudButtonGenerateTip -> "Button 'Generate' generates random cloud key."
    CloudButtonSignKeyTip -> "Button 'SignKey' makes key basing on the sign of connected wallet."
    CloudButtonDeleteTip -> "Button 'Delete' removes currently set key."
    CloudButtonTipDefault -> "To see more details, hover over the active button."
    CloudRestoreTitle -> "Restore all unburned encoins from cloud with your current key" 
    CloudButtonRestore -> "Restore"
    CloudDeleteWindowTitle -> "Delete Cloud Key"
    CloudDeleteWindowContent -> "This action will remove cloud key from the cache! If you won't remember the key you can't recover encoins from remote server! Are you sure?"
    TransferCopySendKeys -> "Скопируйте и отправьте эти ключи вашему получателю по другому каналу связи:"
