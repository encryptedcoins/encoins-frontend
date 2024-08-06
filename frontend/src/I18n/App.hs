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
    | Ok
    | Export
    | ExportTitle
    | ExportName
    | ExportSave
    | ExportAll
    deriving stock (Eq)

instance HasI18n Locale AppMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showAppMessageEn t
    Locale_ZH -> showAppMessageEn t -- TODO: update it

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

